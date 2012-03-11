;; Benedikt Kristinsson
;; Emacs configuration

;; Do we have X? This is false under Debian's emacs-nox package
;; where many features are compiled out
(defvar emacs-has-x (fboundp 'tool-bar-mode))

;; The path to our dotemacs directory
(defvar my-dotemacs "~/repos/dotfiles") 

; Should not be needed!
;; (setq load-path
;;       (append
;;        (mapcar (lambda (x) (concat my-dotemacs x))
;;                     (list "/magit"
;;                           "/org-mode/lisp"
;;                           "/git-commit-mode"
;;                           "/pymacs"
;;                           "/python-mode"
;;                           "/color-theme-tangotango"
;;                           "/rudel"))
;;                     load-path))

	
;; Always follow e.g. ~/.emacs to ~/repos/dotemacs/.emacs
(setq vc-follow-symlinks t)

;; User info
(setq user-full-name "Benedikt Kristinsson")
(setq user-mail-address "benedikt.k@gmail.com")

;; Used in ChangeLog entries
(setq add-log-mailing-address "benedikt.k@gmail.com")

;;; Emacs' interface

(when emacs-has-x 
  ;; GTK stuff
  (custom-set-variables
   '(TeX-PDF-mode t)
   '(column-number-mode t)
   '(doc-view-continuous t)
   '(show-paren-mode t))
  
  
  ;; Tango oblivion (Disabled for the time being, I want white now)
  ;(setq color-theme-load-all-themes nil)
  ;(require 'color-theme-tangotango)
  ;(add-to-list 'load-path "color-theme-tangotango.el")
  ;(color-theme-tangotango)
  ;; (custom-set-faces
  ;; '(default ((t (:weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

  ;; Kill tool, scrollbars and menubar
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0)

)

;;; Emacs

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

;; Don't use the GNU indent, use K&R
(setq c-default-style "k&r" c-basic-offset 4)
;; Enable this for automatic indentaion ("normal") instead of K&R style
;; (add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

;; Multiline commenting in C/C++
(setq comment-style 'multi-line)

;; Use c-mode for .cpp files 
(add-to-list 'auto-mode-alist '("\\.cpp$" . c-mode))

;; Use 4 spaces
(setq default-tab-width 4)
(setq tab-width 4)

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


;; Display the line and column number in the modeline
(setq line-number-mode t)
(setq column-number-mode t)
(line-number-mode t)
(column-number-mode t)

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
(iswitchb-mode 1)
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

;; Use the system default monospaced font in Gnome - Emacs 23.2
;; (Uncommented, seemed to cause emacs to sometimes open a larger window)
;(setq font-use-system-font t)


;; Electric minibuffer!
;; When selecting a file to visit, // will mean / and
;; ~ will mean $HOME regardless of preceding text.
(setq file-name-shadow-tty-properties '(invisible t))
(file-name-shadow-mode 1)

(setq backward-delete-char-untabify nil)


                   ;;;; Forked end ;;;;


;;; Modules

;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode t)

;; org-mode
(when (require 'org-install)
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
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                       interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

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
(global-set-key (kbd "C-c C-s") 'replace-string)
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "C-c C-i") 'indent-region)
(global-set-key (kbd "C-c Q") 'query-replace)
(global-set-key (kbd "C-c q") 'query-replace-regexp)

;; Terminal
(global-set-key (kbd "C-t") (lambda nil (interactive) (ansi-term "/bin/bash")))

;; Home and End keys
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

(define-key emacs-lisp-mode-map (kbd "M-k") 'kill-sexp)


