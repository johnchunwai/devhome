;;; init-multiple-cursors.el --- multiple cursors  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package multiple-cursors
  :commands (mc/add-cursor-on-click)
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this))
  :init
  ;; (global-unset-key (kbd "<C-down-mouse-1>"))
  (bind-keys ("<C-down-mouse-1>" . mc/add-cursor-on-click)
             ("<C-mouse-1>" . ignore)))

(provide 'init-multiple-cursors)

;;; init-multiple-cursors ends here
