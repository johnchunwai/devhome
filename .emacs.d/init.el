;;; init.el --- emacs init file  -*- lexical-binding: t; -*-
;; Prerequisites:
;;     - install dejavu sans mono font from the web
;;     - install cmake, add to PATH
;;     - install LLVM for libclang, add to PATH
;;     - install gnu global for windows, add path (jumping around in src codes)
;;     - install ag (silver searcher)
;;     - install python, pip, virtualevn, virtualenvwrapper (ref: doc/python/py_notes.txt)
;;     - optional: create and work on vritualenv first (ref: doc/python/py_notes.txt)
;;     - install doc/python/elpy-requirements.txt (ref: doc/python/py_notes.txt)

;;; Commentary:
;; Create this while learning emacs. Referencing structure from https://github.com/purcell/emacs.d

;;; Code:

;;;
;;; Sanity checks
;;;
(setq debug-on-error t)

(let ((minver "24")
      (myver "24.5.1"))
  (when (version< emacs-version minver)
    (error "This init file requires at least GNU Emacs %s. Please upgrade." minver))
  (when (version< emacs-version myver)
    (message "This init file is created under GNU Emacs %s. Please upgrade if you experience any issues."
             myver)))

;;;
;;; Bootstrap
;;;
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (let ((default-directory
;;         (my-os-neutral-abs-subdir "site-lisp/" user-emacs-directory)))
;;   (my-create-dir-if-not-exist default-directory)
;;   (normal-top-level-add-subdirs-to-load-path))
(require 'init-utils)
(require 'init-elpa)
(require 'init-use-package)

;;;
;;; user can provide an optional `init-preload-local.el'
;;;
(require 'init-preload-local nil t)

;;;
;;; init packages
;;;
(require 'init-el-get)
(require 'init-hydra)
(require 'init-avy)
(require 'init-ace-link)
(require 'init-ace-window)
(require 'init-golden-ratio)
(require 'init-swiper)
(require 'init-counsel)
(require 'init-smex)
(require 'init-projectile)
(require 'init-yasnippet)
(require 'init-company)
(require 'init-ggtags)
(require 'init-elpy)
(require 'init-irony)

;; package repository

;; for automatic install packages if not already installed on new machines
(defvar my-packages
  '(
    multiple-cursors                    ; multiple points selection
    zenburn-theme                       ; dark theme
    smartparens                         ; smart parentheses
    ))

(defun my-install-packages ()
  "Install all packages defined in `my-packages'."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p my-packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))

;; package configs
(defun my-package-config ()
  (interactive)
  ;; init ag
  (use-package ag)
  ;; multiple-cursors
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-unset-key (kbd "<C-down-mouse-1>"))
  (global-set-key (kbd "<C-down-mouse-1>") 'mc/add-cursor-on-click)
  (global-set-key (kbd "<C-mouse-1>") 'ignore)
  ;; zenburn theme
  (load-theme 'zenburn t)
  (set-face-attribute 'highlight nil :background "#222")
  ;; font - download dejavu sans mono from the web and install
  (set-face-attribute 'default nil :font "DejaVu Sans Mono")
  )

(my-install-packages)


(my-package-config)


;; save/restore session automatically
(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;;;
;;; enable commands by default
;;;
;; enable region uppercase and lowercase
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;
;;; coding
;;;
;; tabs
(setq-default indent-tabs-mode nil)
;; defines how wide to display tabs
(setq-default tab-width 4)
;; defines where the cursor stops when pressing TAB as indent-relative's fallback
(setq tab-stop-list (number-sequence 4 120 4))
(setq-default c-basic-offset 4)
;; google-c-style is download and modified for my own needs
(load "~/.emacs.d/google-c-style-mod")
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(global-subword-mode t)
;;; autocomplete
;; parenthesis stuff
(show-paren-mode 1)
(smartparens-global-mode t)
(require 'smartparens-config)
(global-set-key (kbd "C-0") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-9") 'sp-backward-slurp-sexp)
(global-set-key (kbd "C-}") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-{") 'sp-backward-barf-sexp)

;;;
;;; look and feel
;;;
;; minimalism
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(defalias 'yes-or-no-p 'y-or-n-p)
;; show column position
(setq column-number-mode t)

;;; misc
;; make commands like less, git diff works in shell mode
(setenv "PAGER" "cat")
;; .h to open in c++ mode then c mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; better key bindings
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

(setq delete-by-moving-to-trash t           ; use recycle bin
      save-interprogram-paste-before-kill t ; put clipboard item from other program in kill ring before kill
      require-final-newline t               ; add new line at EOF when save
      apropos-do-all t                      ; apropos checks more
      visible-bell t                        ; flash buffer instead of beep on error
      load-prefer-newer t                   ; load prefers newest version of a file (eg. a.el vs a.elc)
      ;; put all emacs back up files (eg. a.txt~) into same directory
      backup-directory-alist `(("." . ,(my-os-neutral-abs-subdir "backups" user-emacs-directory)))
      )
;; turn on recent file list, call recentf-open-files to list and open
(recentf-mode 1)

(provide 'init)

;;; init.el ends here
