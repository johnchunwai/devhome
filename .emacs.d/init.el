;;; init.el --- emacs init file  -*- lexical-binding: t; -*-
;; Prerequisites:
;;     - install dejavu sans mono font from the web
;;     - install cmake, add to PATH
;;     - install LLVM for libclang, add to PATH

;;; Commentary:
;; Create this while learning emacs.

;;; Code:
(setq debug-on-error t)

(when (version< emacs-version "24.5.1")
  (error "This init file requires at least GNU Emacs 24.5.1, but you're running %s" emacs-version))

;;;
;;; load path
;;;
(require 'cl-lib)

(defun show-file-name ()
  "Show the full path file name in the minibuffer and copy in clipboard."
  (interactive)
  (message (buffer-file-name))
  (kill-new (convert-standard-filename (expand-file-name buffer-file-name))))

(defun my-os-neutral-abs-subdir (subdir dir)
  "Return absolute OS specific path for SUBDIR under DIR"
  (interactive)
  (convert-standard-filename (expand-file-name subdir dir)))

(defun my-create-dir-if-not-exist (dir)
  "Create DIR if not already exist."
  (unless (file-accessible-directory-p dir)
    (make-directory dir)))

;; (let ((default-directory
;;         (my-os-neutral-abs-subdir "site-lisp/" user-emacs-directory)))
;;   (my-create-dir-if-not-exist default-directory)
;;   (normal-top-level-add-subdirs-to-load-path))


;;;
;;; package
;;;
;; package repository
(require 'package)
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(setq package-enable-at-startup nil)
;; initializing packages
(package-initialize)

;; for automatic install packages if not already installed on new machines
;; irony requires cmake to be installed (google), and libclang (google)
(defvar my-packages
  '(
    el-get                              ; allow us to install packages from github and other sources
    multiple-cursors                    ; multiple points selection
    zenburn-theme                       ; dark theme
    yasnippet                           ; template autofill
    company                             ; autocomplete mode
    irony                               ; C++ autocomplete using company and yasnippet
    company-irony                       ; make irony use company mode
    company-irony-c-headers             ; irony autocomplete headers
    flycheck                            ; error checking in real time
    flycheck-irony                      ; error check using irony
    smex                                ; ido style feedback for M-x
    window-numbering                    ; quickly switch window with M-num
    flx-ido                             ; optimized ido with fuzzy search
    ido-ubiquitous                      ; replace stock completion with ido completion everywhere
    idle-highlight-mode                 ; highlight all occurences of word at point on a timer
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

(defun my-irony-mode-hook ()
  "Replace the `completion-at-point' and `complete-symbol' with irony's functions."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

;; package configs
(defun my-package-config ()
  (interactive)
  ;; multiple-cursors
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-unset-key (kbd "<C-down-mouse-1>"))
  (global-set-key (kbd "<C-down-mouse-1>") 'mc/add-cursor-on-click)
  (global-set-key (kbd "<C-mouse-1>") 'ignore)
  ;; zenburn theme
  (load-theme 'zenburn t)
  ;; init window-numbering
  (window-numbering-mode 1)
  ;; init yasnippet
  (yas-global-mode 1)
  ;; (let ((my-snippet-dir (my-os-neutral-abs-subdir "my-snippet" user-emacs-directory)))
  ;;   (yas-load-directory my-snippet-dir))
  (add-hook 'term-mode-hook (lambda () (setq yas-dont-activate t))) ; so tab-complete works in terminal
  ;; company mode for all buffers (optional)
  (global-company-mode)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0)
  ;; init irony
  ;; do M-x irony-install-server when first use
  (setq w32-pipe-read-delay 0)
  ;; When .emacs.d is wiped and irony package version is updated, `irony-install-server' will fail.
  ;; This is because, at least in windows version, cmake cache are stored in system temp directory
  ;; instead of temp/irony-build-<server-version> directory because of the missing slash.
  ;; Even if it uses temp/irony-build-<server-version>, it will still fail if package version updates
  ;; but .emacs wiped. Therefore, we're putting the build folder in irony folder inside `irony-user-dir'.
  (setq irony-server-build-dir (my-os-neutral-abs-subdir (format "build-%s/" (irony-version)) irony-user-dir))
  (dolist (mode '(c++-mode-hook c-mode-hook objc-mode-hook))
    (add-hook mode 'irony-mode))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'irony-eldoc)
  ;; init company-irony
  (with-eval-after-load 'company
    (add-to-list 'company-backends '(company-irony-c-headers company-irony)))
  ;; adds CC special commands to `company-begin-commands' in order to
  ;; trigger completion at interesting places, such as after scope operator
  ;; 	std::
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  ;; init flycheck
  (global-flycheck-mode)
  ;; init flycheck-irony
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  ;; init smex
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
  )

(my-install-packages)
;; get irony-eldoc from https://github.com/johnchunwai/irony-eldoc because
;; the melpa package has bug for new emacs
(require 'el-get)
(el-get-bundle! irony-eldoc
  :description "irony-mode support for eldoc-mode"
  :website "https://github.com/johnchunwai/irony-eldoc"
  :type github
  :feature irony-eldoc
  :pkgname "johnchunwai/irony-eldoc")

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
;;; autocomplete
;; enable ido
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-case-fold t)
(setq ido-use-virtual-buffers t)
(when (equal system-type 'windows-nt)
  (setq ido-max-dir-file-cache 0))      ; windows unreliable directory mod time could cause confusion
(ido-ubiquitous-mode 1)
;; parenthesis stuff
(show-paren-mode 1)
(setq show-paren-style 'expression)
;; (electric-pair-mode 1)
(smartparens-global-mode t)
(require 'smartparens-config)
(global-set-key (kbd "C-0") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-9") 'sp-backward-slurp-sexp)
(global-set-key (kbd "C-}") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-{") 'sp-backward-barf-sexp)
;; init highlight mode
(defun enable-idle-highlight-mode () (idle-highlight-mode t))
(add-hook 'c-mode-common-hook 'enable-idle-highlight-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-idle-highlight-mode)

;;;
;;; look and feel
;;;
;; font - download dejavu sans mono from the web and install
(set-face-attribute 'default nil :font "DejaVu Sans Mono")
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
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face tabs trailing lines-tail)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )