;;; init-smartparens.el --- smart parens  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package smartparens
  :commands (smartparens-global-mode)
  :bind (("C-0" . sp-forward-slurp-sexp)
         ("C-9" . sp-backward-slurp-sexp)
         ("C-}" . sp-forward-barf-sexp)
         ("C-{" . sp-backward-barf-sexp))
  :init
  (smartparens-global-mode t)
  :config
  (require 'smartparens-config)
  )

(provide 'init-smartparens)

;;; init-smartparens ends here
