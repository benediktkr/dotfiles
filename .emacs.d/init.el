;;; dotfiles/emacs --- emacs config for me

;;; User info
(setq user-full-name "Ben Kristinsson")
(setq user-mail-address "ben@sudo.is")
(setq add-log-mailing-address "ben@sudo.is")

;; Set theme
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'wombat t)
;; nord can by loaded "on top of" wombat, looks okay.
;; needs to be installed, the use-package is further down in the files
;;(load-theme 'nord t)

;; disable 'Package cl is deprecated' message.
;; this is coming from some installed package (using loop instead of cl-loop)
(setq byte-compile-warnings '(cl-functions))

(menu-bar-mode -1)
(setq use-dialog-box nil)
(setq inhibit-startup-message 1)
(fset 'yes-or-no-p 'y-or-n-p)
(icomplete-mode 1)
(setq inhibit-splash-screen 1)
(setq initial-major-mode 'markdown-mode)
(setq initial-scratch-message "# Scratch\n\n")
(setq vc-follow-symlinks t)

;; Distance between linum and code
(setq linum-format "%4d \u2502")

;; if a local file is modified from underneath us, ask what to do
;; and dont just reset the buffer. default behaviour over tramp
(global-auto-revert-mode 0)

;; spaces > tabs
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)

;; (require 'no-littering)
;; (setq auto-save-file-name-transforms
;;      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; reduce littering (couldnt get no-littering to work)
(setq package-user-dir
      (expand-file-name (format "bin/elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))
(setq backup-directory-alist '(("." . "~/.emacs.d/var/backup/")))
(setq auto-save-list-file-prefix "~/.emacs.d/var/auto-save/")
(setq transient-history-file     "~/.emacs.d/var/transient/history.el")
(setq transient-levels-file      "~/.emacs.d/var/transient/levels.el")
(setq transient-values-file      "~/.emacs.d/var/transient/values.el")
(setq custom-file                "~/.emacs.d/custom.el")

;; Add better repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (("gnu" . "https://elpa.gnu.org/packages/")
;; ("marmalade" . "https://marmalade-repo.org/packages/")

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; syntax highlight everywhere and mark selections
(transient-mark-mode 1)

;; Match regardless of capitalization
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case t)

;; I don't want stuff like git to start a pager in shell-mode
(setenv "PAGER" "/bin/cat")

;; ansi color in read-only buffers
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

;; Show matching parens immediately
(show-paren-mode 1)
(setq-default show-paren-delay 0.0)

;; When selecting a file to visit, // will mean / and
(setq file-name-shadow-tty-properties '(invisible t))
(file-name-shadow-mode 1)

(setq diredp-hide-details-initially-flag 1)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(defun trailing-ws () (add-hook 'before-save-hook 'delete-trailing-whitespace))

(require 'use-package)

(use-package org-mode
  :mode ("\\.org\\'" . org-mode)
  :bind ("C-c a" . org-agenda)
  :init
  (setq org-log-done 1))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :ensure t)

(use-package vterm
  :init
  (setq vterm-always-compile-module t)
  :ensure t)

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-l")
        (setq lsp-session-file "~/.emacs.d/var/.lsp-session-v1")
        (setq lsp-auto-guess-root t)
  :hook ((python-mode . lsp-deferred)
         (yaml-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)

(use-package which-key
    :config (which-key-mode)
    :ensure t)

(use-package lsp-pyright
  :ensure t
  :init (trailing-ws)
        (auto-insert-mode 1)
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

;; highlights text representing color codes as that color
(use-package rainbow-mode :ensure t :delight)

(use-package ansible           :ensure t)
(use-package company           :ensure t)
(use-package company-ansible   :ensure t)
(use-package dap-mode          :ensure t) ;; debugger (lsp-mode)
(use-package dash              :ensure t)
(use-package epl               :ensure t)
(use-package flycheck          :ensure t)
(use-package flycheck-yamllint :ensure t)
(use-package ibuffer-git       :ensure t)
(use-package ibuffer-vc        :ensure t)
(use-package nginx-mode        :ensure t  :init (trailing-ws))
(use-package nord-theme        :ensure t)
(use-package poetry            :ensure t)

(use-package dockerfile-mode  :ensure t :init (trailing-ws) :mode "Dockerfile")
(use-package groovy-mode      :ensure t :init (trailing-ws) :mode "\\.groovy\\'")
(use-package jenkinsfile-mode :ensure t :init (trailing-ws) :mode "Jenkinsfile")
(use-package jinja2-mode      :ensure t :init (trailing-ws) :mode "\\.j2\\'")
(use-package terraform-mode   :ensure t :init (trailing-ws) :mode "\\.tf\\'")
(use-package yaml-mode        :ensure t :init (trailing-ws):mode ("\\.yaml\\'" "\\.yml\\'"))

(use-package lsp-ui       :ensure t :commands lsp-ui-mode)
(use-package helm-lsp     :ensure t :commands helm-lsp-workspace-symbol)
(use-package lsp-ivy      :ensure t :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

(use-package indent-tools  :ensure t :bind ("C-c >" . indent-tools-hydra/body))
(use-package magit         :ensure t :bind ("C-x g" . magit-status))
(use-package markdown-mode :ensure t
  :bind ("C-j" . markdown-enter-key)
  :init
  (local-set-key (kbd "C-j") 'markdown-enter-key))

(use-package tramp
  :ensure t
  :init
  (setq tramp-default-method "ssh")
  (setq tramp-use-ssh-controlmaster-options nil)
  (use-package ibuffer-tramp
    :ensure t)
  ;; try to speed up tramp
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))
   (setq tramp-auto-save-directory "~/.emacs.d/.clutter/tramp-auto-save/")
   ;; (setq tramp-chunksize 2000)
   (setq tramp-verbose 1))

;; (use-package python
;;   :ensure t
;;   :mode ("\\.py\\'" . python-mode)
;;   ;; :interpreter ("python" . python-mode)
;;   :init
;;   (add-hook 'before-save-hook 'delete-trailing-whitespace)
;;   ;(add-hook 'python-mode-hook 'flycheck-mode)
;;   (setq backward-delete-char-untabify nil))

;; (use-package rust-mode
;;   :ensure t
;;   :mode "\\.rs\\'"
;;   ;;:bind ("C-c C-c" . cargo-process-run)
;;   :init
;;   (add-hook 'rust-mode-hook 'cargo-minor-mode)
;;   (add-hook 'rust-mode-hook 'linum-mode)
;;   (add-hook 'cargo-minor-mode 'visual-line-mode)
;;   (setq rust-format-on-save t))
;;
;; (use-package cargo
;;   :ensure t)
;;
;; (use-package lsp-rust
;;   :ensure t
;;   :after lsp-mode)

;; (use-package flycheck-pycheckers
;;   :ensure t
;;   :init
;;   (global-flycheck-mode 1)
;;   (with-eval-after-load 'flycheck
;;     (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))


(setq c-default-style "k&r" c-basic-offset 4)
(setq comment-style 'multi-line)


;;; Modeline
(setq-default mode-line-format
  (list " "
        ;; */% indicators if the file has been modified
        'mode-line-modified
        ;; the name of the buffer (i.e. filename)
        ;; note this gets automatically highlighted
        'mode-line-buffer-identification
        ;; major and minor modes in effect
        'mode-line-modes
        ;; if which-func-mode is in effect, display which
        ;; function we are currently in.
        '(which-func-mode ("" which-func-format "--"))
        ;; line, column, file %
        'mode-line-position
        ;; if vc-mode is in effect, display version control
        ;; info here
        `(vc-mode vc-mode)
        " @"
        ;; hostname
        'system-name
        ;;; dashes sufficient to fill rest of modeline.
        ;"-%-"
        ))

;; display line number and column number in modeline as (l,c)
(setq-default column-number-mode 1)


;; ;;; Make C-w remove lines without marking them
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy
 a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill
 a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;;; Smash the training wheels
;; (put 'narrow-to-region 'disabled nil)
;; (put 'not-modified 'disabled t)
;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)
;; (put 'erase-buffer 'disabled nil)
;; (put 'dired-find-alternate-file 'disabled nil)

;;; Keybindings

(global-set-key (kbd "C-c M-e") 'eshell)
(global-set-key (kbd "C-c g") 'grep)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-w") 'kill-region)
;(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-c C-r") 'revert-buffer)
(global-set-key (kbd "C-c k") 'browse-kill-ring)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "C-c Q") 'query-replace)
(global-set-key (kbd "C-c q") 'query-replace-regexp)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c C-s") 'sort-paragraphs)
;(global-set-key (kbd "C-c C-s") 'sort-lines)
;(global-set-key (kbd "C-c C-s") 'sort-fields)
(global-set-key (kbd "C-x n") 'next-multiframe-window)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)
(global-set-key (kbd "C-x m") 'manual-entry)
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "M-c") 'comment-region)
(global-set-key (kbd "M-C") 'uncomment-region)
(global-set-key (kbd "C-c l") 'linum-mode)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "C-t") (lambda nil (interactive) (vterm)))

;;; Home and End keys
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

(define-key emacs-lisp-mode-map (kbd "M-k") 'kill-sexp)

(set-default 'truncate-lines t)

;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
;; (add-to-list 'tramp-connection-properties
;;              (list ".*" "locale" "LC_ALL=C"))
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-tramp-set-filter-groups-by-tramp-connection)))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(put 'set-goal-column 'disabled nil)


(auto-insert-mode -1)
(defvar python-skeleton-insert '(""
                                 "#!/usr/bin/env python3" \n
                                 "# " \n
                                 "" \n
                                 "def main():" \n
                                 "pass" \n
                                 "" \n
                                 "if __name__ == \"__main__\":" \n
                                 "main()" ))
(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.\\py\\'" . "python skeleton")
     python-skeleton-insert))
(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.\\py\\.\\j2\\'" . "python skeleton")
     python-skeleton-insert))

;; load even if the file doesnt exist
(load custom-file t)
;;(if (file-exists-p custom-file)
;;  (load custom-file)
;;  (write-region "" nil custom-file))
