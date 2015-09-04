;;; init-cmake-mode.el --- support for CMakeLists.txt  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package cmake-mode
  :commands (cmake-mode)
  :init
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
  :config
  (with-eval-after-load 'company '(add-to-list 'company-backends 'company-cmake))
  )

(provide 'init-cmake-mode)

;;; init-cmake-mode ends here
