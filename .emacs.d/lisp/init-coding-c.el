;;; init-hydra.el --- c coding settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; google-c-style is download and modified for my own needs
(require 'google-c-style-mod)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(setq-default c-basic-offset 4)
;; .h to open in c++ mode then c mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


(provide 'init-coding-c)

;;; init-coding-c ends here
