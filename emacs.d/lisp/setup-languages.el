;;; setup-languages -- Summary
;;; Commentary:
;;; Setup for various languages.
;;; Code:
(require 'use-package)

;; for external tools, need to install:
;; go get code.google.com/p/rog-go/exp/cmd/godef
;; go get code.google.com/p/go.tools/cmd/godoc
(defun cantsin/setup-go ()
  "Set up go."
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "C-c C-f") 'gofmt)))
  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "C-c C-k") 'godoc))))

(use-package go-mode
  :defer t
  :ensure t
  :init (cantsin/setup-go))

(use-package lua-mode
  :defer t
  :ensure t
  :init (setq lua-indent-level 2))

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

(use-package alchemist
  :defer t
  :ensure t
  :init (setq alchemist-project-compile-when-needed t))

(defun cantsin/setup-jedi ()
  "Configure jedi (python)."
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package jedi
  :defer t
  :ensure t
  :config (cantsin/setup-jedi))

;; Github README.mds.
(use-package markdown-mode
  :defer t
  :ensure t
  :init (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

(use-package whitespace
  :ensure t
  :defer t
  :init (setq whitespace-style '(face trailing lines-tail tabs)
                whitespace-line-column 80
                global-whitespace-cleanup-mode t))

(use-package compile
  :defer t
  :ensure t
  :init (setq compilation-scroll-output t))

(use-package restclient
  :defer t
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode)))

;; auto disassemble llvm when opening .bc files
(use-package autodisass-llvm-bitcode
  :ensure t
  :defer t)

(defun load-agda ()
  "Load agda-mode on demand."
  (interactive)
  (condition-case nil
      (load-file (let ((coding-system-for-read 'utf-8))
                   (shell-command-to-string "agda-mode locate")))
    (error nil)))

;; built-ins.
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

(use-package c-mode
  :defer t
  :config (progn
            (setq c-basic-offset 4)
            (c-set-offset 'substatement-open 0)))

(use-package fsharp-mode
  :defer t
  :config (progn
            (setq fsharp-indent-offset 2)))

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

(provide 'setup-languages)
;;; setup-languages.el ends here
