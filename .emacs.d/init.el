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
    use-package                         ; great for managing packages
    multiple-cursors                    ; multiple points selection
    zenburn-theme                       ; dark theme
    yasnippet                           ; template autofill
    company                             ; autocomplete mode
    irony                               ; C++ autocomplete using company and yasnippet
    company-irony                       ; make irony use company mode
    company-irony-c-headers             ; irony autocomplete headers
    flycheck                            ; error checking in real time
    flycheck-irony                      ; error check using irony
    hydra                               ; make emacs bindings that stick around
    avy                                 ; jump to things tree-style
    ace-window                          ; easy select window to switch to
    ace-link                            ; quickly follow links in emacs
    swiper                              ; search with overview and ido replacement that is more efficient
    counsel                             ; convenience replacement functions using ivy
    smartparens                         ; smart parentheses
    golden-ratio                        ; auto resize windows to golden ratio
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

(defun my-ivy-format-function (cands)
  "Add an arrow to the front of current selected candidate among CANDS."
  (let ((i -1))
    (mapconcat
     (lambda (s)
       (concat (if (eq (cl-incf i) ivy--index)
                   "> "
                 "  ")
               s))
     cands "\n")))

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
  ;; init hydra
  ;; hydra is so awesome, check the community wiki for all the hydra snippets to use
  ;; https://github.com/abo-abo/hydra/wiki
  ;; init avy
  (avy-setup-default)
  (setq avy-style 'pre)
  (set-face-attribute 'avy-lead-face-0 nil :foreground "#FFEBCD" :background "#5F9EA0") ; non-term lead char
  (set-face-attribute 'avy-lead-face nil :foreground "#FFEBCD" :background "#5F9EA0") ; lead char
  (set-face-attribute 'avy-lead-face-1 nil :foreground "white" :background "#9370db") ; matched lead char
  ;; (set-face-attribute 'avy-lead-face-2 nil :foreground "white" :background "#f86bf3") ; no idea
  (global-set-key (kbd "M-g g") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  ;; init ace-link
  (ace-link-setup-default)
  (add-hook 'org-mode-hook (lambda () (define-key org-mode-map (kbd "M-o") 'ace-link-org)))
  ;; init ace-window
  (use-package ace-window
    :ensure t
    :defer 1
    :config
    (set-face-attribute 'aw-leading-char-face nil :foreground "deep sky blue" :weight 'bold :height 3.0)
    (set-face-attribute 'aw-mode-line-face nil :inherit 'mode-line-buffer-id :foreground "lawn green")
    (setq aw-keys   '(?a ?s ?d ?f ?j ?k ?l)
          aw-dispatch-always t
          aw-dispatch-alist
          '((?x aw-delete-window     "Ace - Delete Window")
            (?c aw-swap-window       "Ace - Swap Window")
            (?n aw-flip-window)
            (?v aw-split-window-vert "Ace - Split Vert Window")
            (?h aw-split-window-horz "Ace - Split Horz Window")
            (?m delete-other-windows "Ace - Maximize Window")
            (?g delete-other-windows)
            (?b balance-windows)
            (?u winner-undo)
            (?r winner-redo)))

    (when (package-installed-p 'hydra)
      (defhydra hydra-window-size (:color red)
        "Windows size"
        ("h" shrink-window-horizontally "shrink horizontal")
        ("j" shrink-window "shrink vertical")
        ("k" enlarge-window "enlarge vertical")
        ("l" enlarge-window-horizontally "enlarge horizontal"))
      (defhydra hydra-window-frame (:color red)
        "Frame"
        ("f" make-frame "new frame")
        ("x" delete-frame "delete frame"))
      (defhydra hydra-window-scroll (:color red)
        "Scroll other window"
        ("n" joe-scroll-other-window "scroll")
        ("p" joe-scroll-other-window-down "scroll down"))
      (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
      (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
      (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
    (ace-window-display-mode t))
  (global-set-key (kbd "M-p") 'ace-window)
  ;; init swiper
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq confirm-nonexistent-file-or-buffer t)
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))) ; fuzzy completion
  (setq ivy-format-function 'my-ivy-format-function)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-r") 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)
  ;; init counsel
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-h S") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "C-M-s") 'counsel-git-grep)
  ;; init smex
  (setq smex-completion-method 'ivy)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
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
  ;; init golden-ratio
  (require 'golden-ratio)
  (setq golden-ratio-auto-scale t)
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (golden-ratio-mode 1)
  (use-package ggtags
    :config
    (add-hook 'c-mode-common-hook (lambda ()
                                    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                                      (ggtags-mode 1)))))
  (use-package projectile
    :config
    (setq projectile-indexing-method 'alien)
    (with-eval-after-load
        (setq projectile-completion-system 'ivy))
    (projectile-global-mode))

  (use-package elpy
    :init
    (with-eval-after-load 'python (elpy-enable))
    :config
    ;; (setq elpy-rpc-backend "jedi")
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (add-hook 'elpy-mode-hook 'flycheck-mode)))
  )

(my-install-packages)
;; init use-package
;; check out the github page for all info for use-package
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; get irony-eldoc from https://github.com/johnchunwai/irony-eldoc because
;; the melpa package has bug for new emacs
(require 'el-get)
(el-get-bundle! irony-eldoc
  :description "irony-mode support for eldoc-mode"
  :website "https://github.com/johnchunwai/irony-eldoc"
  :type github
  :feature irony-eldoc
  :pkgname "johnchunwai/irony-eldoc")
;; use a custom branch of smex to use ivy backend
(el-get-bundle! smex
  :description "M-x interface with Ido-style fuzzy matching."
  :website "https://github.com/abo-abo/smex"
  :type github
  :feature smex
  :pkgname "abo-abo/smex"
  :post-init (smex-initialize))

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
(global-superword-mode t)
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
