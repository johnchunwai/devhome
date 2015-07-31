(setq debug-on-error t)

;;;
;;; package
;;;
;; package repository
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
;; for automatic install packages if not already installed on new machines
(defvar my-packages
  '(multiple-cursors auto-complete))

(require 'cl-lib)
(defun my-install-packages ()
  ;; Ensure the packages I use are installed. See 'my-packages'.
  (interactive)
  (package-initialize)
  (let ((missing-packages (cl-remove-if #'package-installed-p my-packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))

;; package configs
(defun my-package-config ()
  ;; auto-complete
  (ac-config-default)
  ;; multiple-cursors
  )

(add-hook 'after-init-hook
          (lambda ()
            (my-install-packages)
            (ac-config-default)
            ))



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
(load "~/.emacs.d/google-c-style-mod")
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
;;; autocomplete
;; enable ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
;; text highlighting
(show-paren-mode 1)
(setq show-paren-style 'expression)

;;;
;;; look and feel
;;;
;; get zenburn theme from https://github.com/bbatsov/zenburn-emacs
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs")
(load-theme 'zenburn t)
;; font - download dejavu sans mono from the web and install
(set-face-attribute 'default nil :font "DejaVu Sans Mono")
;; minimalism
(tool-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
;; show column position
(setq column-number-mode t)

;;; misc
;; make commands like less, git diff works in shell mode
(setenv "PAGER" "cat")
;; .h to open in c++ mode then c mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; recycle bin
(setq delete-by-moving-to-trash t)
;; turn on recent file list, call recentf-open-files to list and open
(recentf-mode 1)
