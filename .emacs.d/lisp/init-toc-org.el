;;; init-toc-org.el --- auto generate TOC on save in orgmode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package toc-org
  :init
  (add-hook 'org-mode-hook 'toc-org-mode))

(provide 'init-toc-org)

;;; init-toc-org ends here

