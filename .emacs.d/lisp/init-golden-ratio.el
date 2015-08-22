;;; init-golden-ratio.el --- auto resize windows to golden ratio  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package golden-ratio
  :commands (golden-ratio-mode)
  :init
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale t)
  (when (package-installed-p 'ace-window)
    (add-to-list 'golden-ratio-extra-commands #'ace-window))
  )

(provide 'init-golden-ratio)

;;; init-golden-ratio ends here
