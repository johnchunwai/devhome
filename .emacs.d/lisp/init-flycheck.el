;;; init-flycheck.el --- error checking in real time  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :command (global-flycheck-mode)
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(provide 'init-flycheck)

;;; init-flycheck ends here
