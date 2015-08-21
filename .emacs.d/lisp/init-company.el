;;; init-company --- autocomplete  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package company
  :commands (global-company-mode)
  :init
  (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0)
  )

(provide 'init-company)

;;; init-company ends here
