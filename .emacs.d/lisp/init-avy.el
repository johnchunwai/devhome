;;; init-avy --- jump to things tree-style  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; delay load avy until first used
(use-package avy
  :commands (avy-isearch)
  :bind (("M-g g" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
         ("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2))
  :init
  (bind-key "C-'" 'avy-isearch isearch-mode-map)
  :config
  (setq avy-style 'pre)
  (set-face-attribute 'avy-lead-face-0 nil :foreground "#FFEBCD" :background "#5F9EA0") ; non-term lead char
  (set-face-attribute 'avy-lead-face nil :foreground "#FFEBCD" :background "#5F9EA0") ; lead char
  (set-face-attribute 'avy-lead-face-1 nil :foreground "white" :background "#9370db") ; matched lead char
  ;; (set-face-attribute 'avy-lead-face-2 nil :foreground "white" :background "#f86bf3") ; no idea
  )

(provide 'init-avy)

;;; init-avy ends here
