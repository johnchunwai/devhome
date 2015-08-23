;;; init-smex.el.el --- a smart M-x enhancement  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; use a custom branch of smex to use ivy backend
(el-get-bundle smex
  :description "M-x interface with Ido-style fuzzy matching."
  :website "https://github.com/abo-abo/smex"
  :type github
  :feature smex
  :pkgname "abo-abo/smex")

(use-package smex
  :ensure nil
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :init
  (setq smex-completion-method 'ivy))


(provide 'init-smex)

;;; init-smex ends here
