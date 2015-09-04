;;; init-org.el --- org mode settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :ensure nil
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :config
  (setq org-log-done 'time)
  (setq org-use-sub-superscripts '{})
  (setq org-export-with-sub-superscripts '{})
  )

(provide 'init-org)

;;; init-org ends here
