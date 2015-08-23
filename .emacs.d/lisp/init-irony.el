;;; init-irony.el --- C/C++ minor mode powered by libclang  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; irony requires cmake (google), and libclang (google) to be installed
;; do M-x irony-install-server when first use
(use-package irony
  :commands (irony-mode
             irony-completion-at-point-async
             irony-cdb-autosetup-compile-options)
  :init
  (dolist (mode '(c++-mode-hook c-mode-hook objc-mode-hook))
    (add-hook mode #'irony-mode))
  (defun my-irony-mode-hook ()
    "Replace the `completion-at-point' and `complete-symbol' with irony's functions."
    (bind-key [remap completion-at-point] #'irony-completion-at-point-async irony-mode-map)
    (bind-key [remap complete-symbol] #'irony-completion-at-point-async irony-mode-map)
    (irony-cdb-autosetup-compile-options))
  (add-hook 'irony-mode-hook #'my-irony-mode-hook)
  :config
  (setq w32-pipe-read-delay 0)
  ;; When .emacs.d is wiped and irony package version is updated, `irony-install-server' will fail.
  ;; This is because, at least in windows version, cmake cache are stored in system temp directory
  ;; instead of temp/irony-build-<server-version> directory because of the missing slash.
  ;; Even if it uses temp/irony-build-<server-version>, it will still fail if package version updates
  ;; but .emacs wiped. Therefore, we're putting the build folder in irony folder inside `irony-user-dir'.
  (setq irony-server-build-dir
        (my-os-neutral-abs-subdir (format "build-%s/" (irony-version)) irony-user-dir))
  )

(use-package company-irony-c-headers
  :commands (company-irony-c-headers)
  :init
  (use-package company-irony
    :commands (company-irony company-irony-setup-begin-commands)
    :init
    ;; adds CC special commands to `company-begin-commands' in order to
    ;; trigger completion at interesting places, such as after scope operator
    ;; 	std::
    (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands))
  (with-eval-after-load 'company
    (with-eval-after-load 'irony
      (add-to-list 'company-backends '(company-irony-c-headers company-irony))))
  )

(use-package flycheck-irony
  :commands (flycheck-irony-setup)
  :init
  (with-eval-after-load 'flycheck
    (with-eval-after-load 'irony
      (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
  )

;; get irony-eldoc from https://github.com/johnchunwai/irony-eldoc because
;; the melpa package has bug for new emacs
(el-get-bundle irony-eldoc
  :description "irony-mode support for eldoc-mode"
  :website "https://github.com/johnchunwai/irony-eldoc"
  :type github
  :feature irony-eldoc
  :pkgname "johnchunwai/irony-eldoc")

(use-package irony-eldoc
  :ensure nil
  :commands (irony-eldoc)
  :init
  (add-hook 'irony-mode-hook #'irony-eldoc)
  )

(provide 'init-irony)

;;; init-irony ends here
