;; Benedikt Kristinsson
;; Emacs configuration

;; Do we have X?
(defvar emacs-has-x (fboundp 'tool-bar-mode))

;; Do we care?
(menu-bar-mode -1)

(setq emacs-dir "~/.emacs.d/")

;; Add better repo
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://stable.melpa.org/packages/")))
(package-initialize)
; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install packages
(let ((packages '(magit
                  markdown-mode
                  haskell-mode
                  clojure-mode
                  dash
                  rust-mode
                  neotree
                  terraform-mode
                  yaml-mode
                  ansible
                  dockerfile-mode
                  jinja2-mode
                  groovy-mode)))
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))
;      (add-to-list 'package-selected-packages package)))
;(package-install-selected-packages)


;; Set theme
(load-theme 'wombat t)

(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "dark green")
;;(set-face-background 'mode-line-inactive "black")

;; Modeline
(setq-default mode-line-format
  (list " "
        ; */% indicators if the file has been modified
        'mode-line-modified
        ; the name of the buffer (i.e. filename)
        ; note this gets automatically highlighted
        'mode-line-buffer-identification
        ; major and minor modes in effect
        'mode-line-modes
        ; if which-func-mode is in effect, display which
        ; function we are currently in.
        '(which-func-mode ("" which-func-format "--"))
        ; line, column, file %
        'mode-line-position
        ; if vc-mode is in effect, display version control
        ; info here
        `(vc-mode vc-mode)
        " @"
        ; hostname
        'system-name
        ;; dashes sufficient to fill rest of modeline.
        ;"-%-"
        )
)

;; Distance between linum and code
(setq linum-format "%4d \u2502")


;; Always follow e.g. ~/.emacs to ~/repos/dotemacs/.emacs
(setq vc-follow-symlinks t)

;; User info
(setq user-full-name "Benedikt Kristinsson")
(setq user-mail-address "benedikt@inventati.org")

;; Used in ChangeLog entries
(setq add-log-mailing-address "benedikt@inventati.org")

;;; Emacs' interface

;; Match regardless of capitalization
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case t)

;; Don't display the 'Welcome to GNU Emacs' buffer on startup
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; Remove scratch message
(setq initial-scratch-message nil)

;;; Indenting
;; Use spaces, not tabs
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; Settings for C and C++
(setq c-default-style "k&r" c-basic-offset 4)
(setq comment-style 'multi-line)

;; Load modes based on file extensions
(add-to-list 'auto-mode-alist '("\\.cpp$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.vcl$" . vcl-mode))

;; Use 4 spaces by default for tabbing
(setq default-tab-width 4)
(setq tab-width 4)

;; Force 4 space tabs in text-mode (seems to be ineffective)
(setq tab-stop-list (number-sequence 4 200 4))

                   ;;;; Forked begin ;;;;

;; Make C-w remove lines without marking them
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


;; Don't use graphic dialog boxes
(setq use-dialog-box nil)


;; syntax highlight everywhere
(global-font-lock-mode t)

;; Show matching parens (mixed style)
(show-paren-mode t)
(setq show-paren-delay 0.2)

;; 'mixed highlights the whole sexp making it unreadable, maybe tweak
;; color display?   ←??
(setq show-paren-style 'mixed)

;; Highlight selection
(transient-mark-mode t)

;; make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; Switching
;; (iswitchb-mode -1)
(icomplete-mode 1)

;; Smash the training wheels
(put 'narrow-to-region 'disabled nil)
(put 'not-modified 'disabled t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Backup files
;(setq make-backup-files nil)
;(setq auto-save-default nil)

;; Make C-h a act as C-u C-h a
;(setq apropos-do-all t)

;; I don't want stuff like git to start a pager in shell-mode
(setenv "PAGER" "/bin/cat")

;; Electric minibuffer!
;; When selecting a file to visit, // will mean / and
;; ~ will mean $HOME regppppppardless of preceding text.
(setq file-name-shadow-tty-properties '(invisible t))
(file-name-shadow-mode 1)

;; Colors to improve readability in my dark terminals
(set-face-foreground 'minibuffer-prompt "white")
(set-face-attribute 'link nil :foreground "light blue" :underline t)

(setq backward-delete-char-untabify nil)


                   ;;;; Forked end ;;;;




;;; Modules

;; emacs-lisp-mode
;(add-hook 'emacs-lisp-mode-hook 'eldoc-mode t)

;; org-mode
(when (require 'org)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t))

;; rudel
; require returns nil if not found
(when (require 'rudel-loaddefs nil t)
  (global-rudel-minor-mode 1))

;(require 'rudel-loaddefs)

;(load-file "rudel/rudel-loaddefs.el")
;(global-rudel-minor-mode 1)

;; magit
(autoload 'magit-status "magit" nil t)

;; tramp
(setq tramp-default-method "ssh")

;; python-mode
;(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;(setq interpreter-mode-alist (cons '("python" . python-mode)
;                                       interpreter-mode-alist))
;(autoload 'python-mode "python-mode" "Python editing mode." t)

;;; Keybindings
;; GIT
(global-set-key (kbd "C-x g") 'magit-status)

;; Misc
(global-set-key (kbd "C-c M-e") 'eshell)
(global-set-key (kbd "C-c S") 'ispell-change-dictionary)
(global-set-key (kbd "C-c g") 'grep)

;; Buffers
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
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "C-x n") 'next-multiframe-window)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)
(global-set-key (kbd "C-x m") 'manual-entry)
(global-set-key (kbd "M-c") 'comment-region)
(global-set-key (kbd "M-C") 'uncomment-region)
(global-set-key (kbd "C-c n") 'neotree-toggle)
(global-set-key (kbd "C-c l") 'linum-mode)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Terminal
(global-set-key (kbd "C-t") (lambda nil (interactive) (ansi-term "/bin/zsh")))

;; Home and End keys
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

(define-key emacs-lisp-mode-map (kbd "M-k") 'kill-sexp)

;; Useful things for python
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:setup-keys t)                      ; optional
;; (setq jedi:complete-on-dot t)                 ; optional

; On-the-fly pyflakes checking
; shows errors in the minibuffer when highlighted (http://bitbucket.org/brodie/dotfiles/src/tip/.emacs.d/plugins/flymake-point.el)
;; (require 'flymake-point "~/.emacs.d/flymake-point.el")
;; (setq python-check-command "pyflakes")
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pyflakes" (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))

;; For when i need to work with peculiar people :)
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (when (string-match "dohop" (buffer-file-name))
;;               (when (not (string-match "varnish" (buffer-file-name)))
;;                 (setq indent-tabs-mode t)
;;                 (setq tab-width 4)
;;                 (setq python-indent 4)))
;;             (flymake-mode 1)))

(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("b19b642b0d5be8ec4bc96698260575d3eb81a22064911a8036213facf3a9a6fa" default)))
 '(doc-view-continuous t)
 '(package-selected-packages
   (quote
    (flycheck-pycheckers flycheck groovy-mode jinja2-mode ansible yaml-mode terraform-mode neotree markdown-mode magit haskell-mode epl dockerfile-mode company clojure-mode cargo)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(ido-mode -1)

;; vcl-mode things
(setq vcl-indent-level 4)

;; Groovy-mode bug, won't load unless this is required first.
(require 'cl)

;; Rust stuff goes here I guess
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook 'linum-mode)
(add-hook 'cargo-minor-mode 'visual-line-mode)

;; ansi color in read-only buffers
(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

;; Remove trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; neotree
(require 'neotree)
(setq-default neo-show-hidden-files t)
(setq neo-smart-open t)

;; try to speed up tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-verbose 1)
