;;; init-elpa.el --- init package repo and stuff  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; repo - basically, we just use melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)


;;; utils
(defun my-package-maybe-enable-signatures ()
  "Conditionally enable signature check depending on if gpg is found.

If gpg cannot be found, signature checking will fail, so we conditionally
enable it according to whether gpg is available.  We re-run this check once
$PATH has been configured."
  (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

(defun my-require-package (package &optional min-version no-refresh)
  "Install given PACKAGE.

Optionally requiring MIN-VERSION.  If NO-REFRESH is non-nil, the available
package lists will not be re-downloaded in order to locate PACKAGE."
  (unless (package-installed-p package min-version)
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (package-refresh-contents)
      (my-require-package package min-version t))))


;;; fire up
(my-package-maybe-enable-signatures)
(setq package-enable-at-startup nil)
(package-initialize)


(provide 'init-elpa)

;;; init-elpa ends here
