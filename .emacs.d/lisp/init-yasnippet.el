;;; init-yasnippet --- template autofill  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :commands (yas-global-mode)
  :init
  (yas-global-mode 1)
  :config
  ;; so tab completion works in terminal
  (defun my-yas-term-mode-hook () (setq yas-dont-activate t))
  (add-hook 'term-mode-hook #'my-yas-term-mode-hook t)
  ;; (yas-load-directory (my-os-neutral-abs-subdir "my-snippet" user-emacs-directory))
  )

(provide 'init-yasnippet)

;;; init-yasnippet ends here
