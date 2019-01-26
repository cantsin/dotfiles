;;; setup-lisp -- Summary
;;; Commentary:
;;; Setup for various lisps.
;;; Code:
(require 'use-package)

(use-package paredit
  :defer t
  ;; make paredit and eldoc play nice.
  :config (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun cantsin/setup-elisp ()
  "Set up elisp."
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (use-package elisp-slime-nav
    :defer t
    :diminish t
    :config (add-hook 'emacs-lisp-mode-hook
                      (lambda () (elisp-slime-nav-mode t)))))

(use-package eldoc
  :defer t)

(use-package elisp
  :defer t
  :bind (("C-c v" . eval-buffer)
         ("C-c C-e" . eval-and-replace))
  :config (cantsin/setup-elisp))

(defun cantsin/setup-cider ()
  "Set up cider."
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-mode-hook 'subword-mode)
  (add-hook 'cider-mode-hook 'paredit-mode)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (setq cider-show-error-buffer nil)
  (add-to-list 'same-window-buffer-names "*cider*"))

(use-package cider
  :defer t
  :ensure t
  :config (cantsin/setup-cider))

(defun cantsin/setup-paren ()
  "Set up paren."
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  (setq show-paren-style 'mixed))

(use-package paren
  :defer t
  :config (cantsin/setup-paren))

(provide 'setup-lisp)
;;; setup-lisp.el ends here
