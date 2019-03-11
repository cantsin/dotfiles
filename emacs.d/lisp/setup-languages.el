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

(defun my-makefile-hook ()
  "Use tabs."
  (setq indent-tabs-mode t))
(add-hook 'makefile-mode-hook 'my-makefile-hook)

(use-package c-mode
  :defer t
  :config (progn
            (setq c-basic-offset 4)
            (c-set-offset 'substatement-open 0)))

(use-package fsharp-mode
  :defer t
  :config (progn
            (setq fsharp-indent-offset 2)))

(use-package racer-mode
  :defer t)

(use-package rust-mode
  :defer t
  :init
  (progn (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
         (add-hook 'racer-mode-hook #'eldoc-mode)
         (add-hook 'racer-mode-hook #'company-mode)
         (add-hook 'rust-mode-hook #'racer-mode)
         (setq rust-format-on-save t)))

(use-package nix-update
  :defer t)

(provide 'setup-languages)
;;; setup-languages.el ends here
