;;; init-theme.el --- a nice dark theme  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package zenburn-theme
  :init
  (load-theme 'zenburn t)
  (set-face-attribute 'highlight nil :background "#222")
  ;; font - download dejavu sans mono from the web and install
  (set-face-attribute 'default nil :font "DejaVu Sans Mono")
  )

(provide 'init-theme)

;;; init-theme ends here
