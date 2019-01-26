;;; setup-haskell -- Summary
;;; Commentary:
;;; Setup for Haskell and related.
;;; Code:
(require 'use-package)

(defun cantsin/setup-elm ()
  "Set up Elm."
  (setq elm-tags-on-save t)
  (setq elm-tags-exclude-elm-stuff nil)
  (setq elm-format-on-save t)
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-to-list 'company-backends 'company-elm))

(use-package elm-mode
  :defer t
  :ensure t
  :config (cantsin/setup-elm))

(defun flymake-haskell-init ()
  "Generate a tempfile, run `hslint` on it, and delete file."
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "hslint" (list local-file))))

(defun flymake-haskell-enable ()
  "Enables 'flymake-mode' for haskell."
  (when (and buffer-file-name
             (file-writable-p
              (file-name-directory buffer-file-name))
             (file-writable-p buffer-file-name))
    (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line)
    (flymake-mode t)))

(defun cantsin/setup-haskell ()
  "Set up haskell."
  (use-package haskell-interactive-mode
    :defer t)
  (use-package haskell-process
    :defer t)
  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-tags-on-save t)
   '(haskell-process-type 'cabal-repl))
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (eval-after-load 'haskell-mode
    '(progn
       (require 'flymake)
       (push '("\\.l?hs\\'" flymake-haskell-init) flymake-allowed-file-name-masks)
       (add-hook 'haskell-mode-hook 'flymake-haskell-enable)))
  (eval-after-load 'haskell-mode
    '(progn
       (load-library "inf-haskell")
       (defun my-inf-haskell-hook ()
         (setq comint-prompt-regexp
               (concat comint-prompt-regexp "\\|^.> ")))
       (add-to-list 'inferior-haskell-mode-hook 'my-inf-haskell-hook))))

(use-package haskell-mode
  :ensure t
  :defer t
  :config (cantsin/setup-haskell))

(provide 'setup-haskell)
;;; setup-haskell.el ends here
