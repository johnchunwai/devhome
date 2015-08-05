(setq debug-on-error t)

;;;
;;; load path
;;;
(require 'cl-lib)
(let ((default-directory
        (convert-standard-filename (concat user-emacs-directory "site-lisp/"))))
  (normal-top-level-add-subdirs-to-load-path))

;;;
;;; package
;;;
;; package repository
(require 'package)
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(setq package-enable-at-startup nil)
;; benchmark-init - start as early as possible because we need to benchmark init
;; (let ((benchmark-directory-list
;;        (file-expand-wildcards (convert-standard-filename (concat user-emacs-directory "elpa/benchmark-init*"))))
;;       (found-benchmark-init nil))
;;   (dolist (file benchmark-directory-list)
;;     (when (file-directory-p file)
;;       (add-to-list 'load-path file)
;;       (setq found-benchmark-init t)))
;;   (when found-benchmark-init
;;     (require 'benchmark-init)
;;     (benchmark-init/activate)))
;; initializing packages
(package-initialize)

;; for automatic install packages if not already installed on new machines
;; irony requires cmake to be installed (google), and libclang (google)
(defvar my/packages
  '(multiple-cursors zenburn-theme yasnippet company irony company-irony
                     company-irony-c-headers flycheck flycheck-irony
                     smex))

(defun my/install-packages ()
  ;; Ensure the packages I use are installed. See 'my/packages'.
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p my/packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my/irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

;; package configs
(defun my/package-config ()
  ;; multiple-cursors
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-unset-key (kbd "<C-down-mouse-1>"))
  (global-set-key (kbd "<C-down-mouse-1>") 'mc/add-cursor-on-click)
  (global-set-key (kbd "<C-mouse-1>") 'ignore)
  ;; zenburn theme
  (load-theme 'zenburn t)
  ;; init yasnippet
  (yas-global-mode 1)
  ;; company mode for all buffers (optional)
  (global-company-mode)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0)
  ;; init irony
  ;; do M-x irony-install-server when first use
  (dolist (mode '(c++-mode-hook c-mode-hook objc-mode-hook))
    (add-hook mode 'irony-mode))
  (add-hook 'irony-mode-hook 'my/irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  ;; get irony-eldoc from https://github.com/josteink/irony-eldoc because
  ;; the melpa package has bug for new emacs
  (require 'irony-eldoc)
  (add-hook 'irony-mode-hook 'irony-eldoc)
  (setq w32-pipe-read-delay 0)
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

(my/install-packages)
(my/package-config)


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
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
;; text highlighting
(show-paren-mode 1)
(setq show-paren-style 'expression)

;;;
;;; look and feel
;;;
;; font - download dejavu sans mono from the web and install
(set-face-attribute 'default nil :font "DejaVu Sans Mono")
;; minimalism
(when (fboundp tool-bar-mode)
  (tool-bar-mode -1))
(when (scroll-bar-mode)
  (scroll-bar-mode -1))
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
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      )
;; turn on recent file list, call recentf-open-files to list and open
(recentf-mode 1)
