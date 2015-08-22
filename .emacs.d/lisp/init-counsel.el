;;; init-counsel.el --- convenience replacement functions using ivy  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("<f1> v" . counsel-describe-variable)
         ("C-x C-f" . counsel-find-file)
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-M-s" . counsel-git-grep)))

(provide 'init-counsel)

;;; init-counsel ends here
