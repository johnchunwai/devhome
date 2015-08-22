;;; init-projectile.el --- project navigation and management  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :commands (projectile-global-mode)
  :init
  (projectile-global-mode)
  :config
  (setq projectile-indexing-method 'alien)
  (setq projectile-completion-system 'ivy)
  )

(provide 'init-projectile)

;;; init-projectile ends here
