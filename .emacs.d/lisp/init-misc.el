;;; init-misc.el --- misc settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; enable some commands by default
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; some alias and keybindings
(defalias 'yes-or-no-p 'y-or-n-p)
(bind-key "C-x C-b" #'ibuffer-other-window)

;; max frame on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; make commands like less, git diff works in shell mode
(setenv "PAGER" "cat")

;; some global settings
(setq delete-by-moving-to-trash t           ; use recycle bin
      save-interprogram-paste-before-kill t ; put clipboard item from other program in kill ring before kill
      require-final-newline t               ; add new line at EOF when save
      apropos-do-all t                      ; apropos checks more
      visible-bell t                        ; flash buffer instead of beep on error
      load-prefer-newer t                   ; load prefers newest version of a file (eg. a.el vs a.elc)
      column-number-mode t                  ; show column position
      ;; put all emacs back up files (eg. a.txt~) into same directory
      backup-directory-alist `(("." . ,(my-os-neutral-abs-subdir "backups" user-emacs-directory)))
      ;; defines where the cursor stops when pressing TAB as indent-relative's fallback
      tab-stop-list (number-sequence 4 120 4)
      inhibit-splash-screen t               ; disable splash screen
      )

;; minimalism
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; turn on recent file list, call recentf-open-files to list and open
(recentf-mode 1)

(provide 'init-misc)

;;; init-misc ends here
