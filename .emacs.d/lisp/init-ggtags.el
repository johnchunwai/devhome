;;; init-ggtags.el --- frontend to gnu global  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ggtags
  :commands (ggtags-mode)
  :init
  (add-hook 'c-mode-common-hook (lambda ()
                                  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                                    (ggtags-mode 1)))))


(provide 'init-ggtags)

;;; init-ggtags ends here
