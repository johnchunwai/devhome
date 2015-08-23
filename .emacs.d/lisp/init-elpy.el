;;; init-elpy.el --- python dev env  -*- lexical-binding: t; -*-

;;; Commentary:
;;     - install doc/python/elpy-requirements.txt (ref: doc/python/py_notes.txt)

;;; Code:

(use-package elpy
  :commands (elpy-enable)
  :init
  (with-eval-after-load 'python (elpy-enable))
  :config
  ;; (setq elpy-rpc-backend "jedi")
  (when (require 'flycheck nil t)
    (remove-hook 'elpy-modules #'elpy-module-flymake)
    (add-hook 'elpy-mode-hook #'flycheck-mode)))

(provide 'init-elpy)

;;; init-elpy ends here
