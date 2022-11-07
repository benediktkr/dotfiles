;;; dotfiles/emacs --- emacs config for me

;;; User info
(setq user-full-name "Benedikt Kristinsson")
(setq user-mail-address "benedikt@inventati.org")
(setq add-log-mailing-address "benedikt@inventati.org")


;; if a local file is modified from underneath us, ask what to do
;; and dont just reset the buffer. default behaviour over tramp
(global-auto-revert-mode 0)

;; Add better repos
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
;; ("marmalade" . "https://marmalade-repo.org/packages/")

(setq diredp-hide-details-initially-flag 1)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;; Emacs settings
(menu-bar-mode -1)
(setq backup-directory-alist '(("." . "~/.saves")))
(setq use-dialog-box nil)
(setq inhibit-startup-message 1)
(fset 'yes-or-no-p 'y-or-n-p)
(icomplete-mode 1)
(setq inhibit-splash-screen 1)
(setq initial-major-mode 'markdown-mode)
(setq initial-scratch-message "# Scratch

")
(setq vc-follow-symlinks t)
;; Distance between linum and code
(setq linum-format "%4d \u2502")

;; syntax highlight everywhere and mark selections
(transient-mark-mode 1)

;; spaces > tabs
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)

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


(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(use-package ansible
  :defer 3
  :ensure t)

;; (use-package cargo
;;   :ensure t)

(use-package clojure-mode
  :mode "\\.clj\\'"
  :config
  (message "loaded clojure-mode")
  :ensure t)

(use-package dash
  :defer 3
  :ensure t)

(use-package dockerfile-mode
  :mode "Dockerfile"
  :config
  (message "loaded dockerfile-mode")
  :ensure t)

(use-package epl
  :defer 3
  :ensure t)

(use-package groovy-mode
  :mode "\\.groovy\\'"
  :ensure t
  :config
  (message "loaded groovy-mode"))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :config
  (message "loaded haskell-mode"))

(use-package jinja2-mode
  :ensure t
  :mode "\\.j2\\'"
  :config
  (message "loaded jinja2-mode"))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (message "loaded magit"))

(use-package markdown-mode
  :ensure t
  :bind ("C-j" . markdown-enter-key)
  :init
  (local-set-key (kbd "C-j") 'markdown-enter-key)
  :config
  (message "loaded markdown-mode"))

(use-package neotree
  :ensure t
  :bind ("C-c n" . neotree-toggle)
  :config
  (message "starting to load neotree")
  (setq-default neo-show-hidden-files t)
  (setq neo-smart-open t)
  (message "loaded neotree"))

;; (use-package nord-theme
;;   :ensure t
;; )

(use-package org-mode
  :mode ("\\.org\\'" . org-mode)
  :bind ("C-c a" . org-agenda)
  :init
  (setq org-log-done 1)
  :config
  (message "loaded org-mode"))

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  ;; :interpreter ("python" . python-mode)
  :init
  (message "loading python-mode")
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  ;(add-hook 'python-mode-hook 'flycheck-mode)
  (setq backward-delete-char-untabify nil)
  :config
  (message "loaded python-mode"))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  ;;:bind ("C-c C-c" . cargo-process-run)
  :init
  (message "loading rust-mode")
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook 'linum-mode)
  (add-hook 'cargo-minor-mode 'visual-line-mode)
  (setq rust-format-on-save t)
  :config
  (message "loaded rust-mode"))

;; (use-package lsp-mode
;;   :ensure t
;;   :init
;;   (add-hook 'prog-mode-hook 'lsp-mode)
;;   :config
;;   (use-package lsp-flycheck
;;     :after flycheck))

;; (use-package lsp-rust
;;   :ensure t
;;   :after lsp-mode)

(use-package indent-tools
  :ensure t
  :init (require 'indent-tools)
  :bind ("C-c >" . indent-tools-hydra/body))

(use-package tramp
  :ensure t
  :init
  (setq tramp-default-method "ssh")
  (setq tramp-use-ssh-controlmaster-options nil)
  ;; try to speed up tramp
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))
  (setq tramp-auto-save-directory "~/.saves/tramp/")
  ;; (setq tramp-chunksize 2000)
  (setq tramp-verbose 1)
  (use-package ibuffer-tramp
    :config
    (message "ibuffer-tramp loaded"))
  (use-package ibuffer-vc
    :config
    (message "ibuffer-vc-loaded"))

  :config
  (message "loaded tramp"))

(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'"
  :config
  (message "loaded terraform-mode"))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :config
  ;; (add-hook 'yaml-mode-hook 'flycheck-mode)
  ;; (add-hook 'yaml-mode-hook 'flyspell-mode)
  (message "loaded yaml-mode"))
  ;; (use-package flycheck-yamllint
  ;;   :ensure t
  ;;   :init
  ;;   (progn
  ;;     (eval-after-load 'flycheck
  ;;       '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup)))
  ;;   :config
  ;;   (message "loaded flycheck-yamllint")))

;; (use-package flycheck
;;   :ensure t
;;   :init
;;   (global-flycheck-mode)
;;   :config
;;   (message "flycheck loaded"))

;; (use-package flycheck-pycheckers
;;   :ensure t
;;   :init
;;   (global-flycheck-mode 1)
;;   (with-eval-after-load 'flycheck
;;     (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))
;;   :config
;;   (message "flycheck-pycheres loaded"))



;; packages that use-package cant use, maybe delete
(setq c-default-style "k&r" c-basic-offset 4)
(setq comment-style 'multi-line)

;; Set theme
;; theme looks nicer if i load wombat first, to get the background color
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'wombat t)
;;(load-theme 'nord t)

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


;;; Make C-w remove lines without marking them
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
(global-set-key (kbd "C-c C-s") 'sort-lines)
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

;; write elisp properly, its emacs after all
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Use spaces, not tabs.
            (setq indent-tabs-mode nil)
            ;; Keep M-TAB for `completion-at-point'
            (define-key flyspell-mode-map "\M-\t" nil)
            ;; Pretty-print eval'd expressions.
            (define-key emacs-lisp-mode-map
              "\C-x\C-e" 'pp-eval-last-sexp)
            ;; Recompile if .elc exists.
            (add-hook (make-local-variable 'after-save-hook)
                      (lambda ()
                        (byte-force-recompile default-directory)))
            (define-key emacs-lisp-mode-map
              "\r" 'reindent-then-newline-and-indent)))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
;; (add-to-list 'tramp-connection-properties
;;              (list ".*" "locale" "LC_ALL=C"))
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-tramp-set-filter-groups-by-tramp-connection)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4ea1959cfaa526b795b45e55f77724df4be982b9cd33da8d701df8cdce5b2955" default))
 '(package-selected-packages
   '(ibuffer-git nginx-mode rainbow-identifiers rainbow-blocks rainbow-mode jenkinsfile-mode php-mode indent-tools highlight-indentation yaml-mode use-package terraform-mode pyflakes nord-theme neotree markdown-mode magit jinja2-mode ibuffer-vc ibuffer-tramp haskell-mode groovy-mode dockerfile-mode company clojure-mode cargo ansible)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)

(auto-insert-mode 1)
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
;; added to silennce the linter
(provide '.emacs)
;;; .emacs ends here
