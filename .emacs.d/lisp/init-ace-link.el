;;; init-ace-link.el --- quickly follow links in emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ace-link
  :commands (ace-link-setup-default)
  :init
  (ace-link-setup-default)
  :config
  (defun my-ace-org-mode-hook ()
    (bind-key "M-o" #'ace-link-org org-mode-map))
  (add-hook 'org-mode-hook #'my-ace-org-mode-hook)
  )

(provide 'init-ace-link)

;;; init-ace-link ends here
