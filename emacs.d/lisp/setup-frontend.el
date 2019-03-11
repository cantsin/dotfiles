;;; setup-frontend -- Summary
;;; Commentary:
;;; Setup for the front end
;;; Code:
(require 'use-package)

(use-package js2-mode
  :defer t
  :ensure t
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
          (setq-default js2-basic-offset 2)
          (setq js-indent-level 2)
          (add-hook 'js-mode-hook 'js2-minor-mode)))

(defun cantsin/init-web ()
  "Set up web-mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode)))

(use-package web-mode
  :defer t
  :init (cantsin/init-web)
  :config (smartparens-mode 0))

(defun cantsin/init-tide ()
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (setq typescript-indent-level 2)
  (setq company-idle-delay 0)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package tide-mode
  :defer t
  :init (cantsin/init-tide))

(use-package nodejs-repl
  :defer t
  :config (setq nodejs-repl-arguments
                 '("--use-strict"
                   "--es_staging"
                   "--harmony"
                   "--harmony_shipping"
                   "--harmony_modules"
                   "--harmony_array_includes"
                   "--harmony_regexps"
                   "--harmony_arrow_functions"
                   "--harmony_proxies"
                   "--harmony_sloppy"
                   "--harmony_tostring")))

(provide 'setup-frontend)
;;; setup-frontend.el ends here
