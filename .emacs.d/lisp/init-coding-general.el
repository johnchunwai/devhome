;;; init-coding-general.el --- general coding settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(show-paren-mode 1)

(global-subword-mode 0)

(setq-default
 whitespace-line-column 80
 whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook #'whitespace-mode)

(provide 'init-coding-general)

;;; init-coding-general ends here
