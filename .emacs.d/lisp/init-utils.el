;;; init-utils.el --- utility functions  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'cl-lib)

(defun my-show-file-name ()
  "Show the full path file name in the minibuffer and copy in clipboard."
  (interactive)
  (message (buffer-file-name))
  (kill-new (convert-standard-filename (expand-file-name buffer-file-name))))

(defun my-os-neutral-abs-subdir (subdir dir)
  "Return absolute os specific path for SUBDIR under DIR."
  (interactive)
  (convert-standard-filename (expand-file-name subdir dir)))

(defun my-create-dir-if-not-exist (dir)
  "Create DIR if not already exist."
  (unless (file-accessible-directory-p dir)
    (make-directory dir)))

(provide 'init-utils)

;;; init-utils ends here
