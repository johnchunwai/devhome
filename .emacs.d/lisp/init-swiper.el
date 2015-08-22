;;; init-swiper.el --- great ido replacement  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package swiper
  :commands (ivy-mode)
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq confirm-nonexistent-file-or-buffer t)
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))) ; fuzzy completion
  (setq ivy-format-function 'my-ivy-format-function)
  (bind-key "TAB" #'ivy-partial ivy-minibuffer-map))


(provide 'init-swiper)

;;; init-swiper ends here
