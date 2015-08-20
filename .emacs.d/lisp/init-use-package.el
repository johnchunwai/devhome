;;; init-use-package --- init use-package  -*- lexical-binding: t; -*-

;;; Commentary:
;; Must be done before all other packages as they all use this.

;;; Code:

(my-require-package 'use-package)
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t)
  (setq use-package-verbose t))


(provide 'init-use-package)

;;; init-use-package ends here
