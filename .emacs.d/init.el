;;; init.el --- emacs init file  -*- lexical-binding: t; -*-
;; Prerequisites:
;;     - install dejavu sans mono font from the web
;;     - install cmake, add to PATH
;;     - install LLVM for libclang, add to PATH
;;     - install gnu global for windows, add path (jumping around in src codes)
;;     - install ag (silver searcher)
;;     - install python, pip, virtualevn, virtualenvwrapper (ref: doc/python/py_notes.txt)
;;     - optional: create and work on vritualenv first (ref: doc/python/py_notes.txt)
;;     - install doc/python/elpy-requirements.txt (ref: doc/python/py_notes.txt)

;;; Commentary:
;; Create this while learning emacs. Referencing structure from https://github.com/purcell/emacs.d

;;; Code:

;;;
;;; Sanity checks
;;;
(setq debug-on-error t)

(let ((minver "24")
      (myver "24.5.1"))
  (when (version< emacs-version minver)
    (error "This init file requires at least GNU Emacs %s. Please upgrade." minver))
  (when (version< emacs-version myver)
    (message "This init file is created under GNU Emacs %s. Please upgrade if you experience any issues."
             myver)))

;;;
;;; Bootstrap
;;;
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (let ((default-directory
;;         (my-os-neutral-abs-subdir "site-lisp/" user-emacs-directory)))
;;   (my-create-dir-if-not-exist default-directory)
;;   (normal-top-level-add-subdirs-to-load-path))
(require 'init-utils)
(require 'init-elpa)
(require 'init-use-package)

;;;
;;; user can provide an optional `init-preload-local.el'
;;;
(require 'init-preload-local nil t)

;;;
;;; init packages
;;;
(require 'init-el-get)
(require 'init-misc)
(require 'init-ag)
(require 'init-hydra)
(require 'init-avy)
(require 'init-ace-link)
(require 'init-ace-window)
(require 'init-golden-ratio)
(require 'init-swiper)
(require 'init-counsel)
(require 'init-smex)
(require 'init-multiple-cursors)
(require 'init-coding-general)
(require 'init-coding-c)
(require 'init-projectile)
(require 'init-smartparens)
(require 'init-yasnippet)
(require 'init-company)
(require 'init-ggtags)
(require 'init-elpy)
(require 'init-irony)
(require 'init-theme)
(require 'init-desktop-save)

;;;
;;; user can provide an optional `init-local.el'
;;;
(require 'init-local nil t)

(provide 'init)

;;; init.el ends here
