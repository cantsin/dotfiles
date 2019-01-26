;;; setup-buffer -- summary
;;; Commentary:
;;; set various buffer settings
;;; Code:
(require 'use-package)

;; wrapping.
(add-hook 'text-mode-hook
          'turn-on-auto-fill)
(add-hook 'markdown-mode-hook
          '(lambda ()
             (font-lock-add-keywords nil '(("\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\"" 0 font-lock-string-face)))
             (writegood-mode)))
(add-hook 'html-mode-hook
          '(lambda ()
             (turn-off-auto-fill)
             (visual-line-mode)))

;;;; NB this breaks latest helm so disabling for now
;; turn off annoying ffap behavior
;; (use-package ffap
;;   :defer t
;;   :config (setq ffap-alist nil
;;                 ffap-machine-p-known 'accept
;;                 ffap-require-prefix nil
;;                 ffap-gopher-regexp nil
;;                 ffap-url-regexp nil
;;                 ffap-ftp-regexp nil
;;                 ffap-ftp-sans-slash-regexp nil
;;                 ffap-rfs-regexp nil
;;                 ffap-shell-prompt-regexp nil))
;; (defun ffap-file-at-point nil
;;   "Turn off ffap file-at-point completely."
;;   nil)

(use-package windmove
  :defer t
  :ensure t)

;; (use-package framemove
;;   :ensure t
;;   :defer t
;;   :config (setq framemove-hook-into-windmove t))

;; save buffers on buffer switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  "Switch to buffer."
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  "Other window."
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  "Move up."
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  "Move down."
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  "Move left."
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  "Move right."
  (when buffer-file-name (save-buffer)))

;; save point.
(use-package saveplace
  :ensure t
  :config (progn
            (setq-default save-place t)
            (setq save-place-file (concat user-emacs-directory "places"))))

(defun bjm-deft-save-windows (orig-fun &rest args)
  "Advice to save windows -- ORIG-FUN ARGS."
  (setq bjm-pre-deft-window-config (current-window-configuration))
  (apply orig-fun args))

(defun bjm-quit-deft ()
  "Save buffer, kill buffer, kill deft buffer, and restore window config to the way it was before deft was invoked."
  (interactive)
  (save-buffer)
  (kill-this-buffer)
  (switch-to-buffer "*Deft*")
  (kill-this-buffer)
  (when (window-configuration-p bjm-pre-deft-window-config)
    (set-window-configuration bjm-pre-deft-window-config)))

(use-package deft
  :init (advice-add 'deft :around #'bjm-deft-save-windows)
  :bind (("C-c q" . bjm-quit-deft)
         ("C-c d" . deft-new-file)))

(defun cantsin/markdown-init ()
  "Initialize markdown."
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (turn-off-auto-fill)
               (visual-line-mode)))
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode))

(defun cantsin/markdown-config ()
  "Set up markdown."
  (defconst markdown-regex-footnote-inline
    "\\(\\^\\[.+?\\]\\)"
    "Regular expression for a footnote inline marker ^[fn].")
  (defface markdown-footnote-inline-face
    '((t (:inherit font-lock-keyword-face)))
    "Face for footnote markers."
    :group 'markdown-faces)
  (add-to-list 'markdown-mode-font-lock-keywords-basic
               (cons markdown-regex-footnote-inline 'markdown-footnote-face)))

(use-package markdown
  :defer t
  :init (cantsin/markdown-init)
  :config (cantsin/markdown-config))

(use-package avy
  :bind (("C-c j" . avy-goto-word-or-subword-1)
         ("s-y" . ace-window)))

(defmacro def-pairs (pairs)
  `(progn
     ,@(cl-loop for (key . val) in pairs
          collect
            `(defun ,(read (concat
                            "wrap-with-"
                            (prin1-to-string key)
                            "s"))
                 (&optional arg)
               (interactive "p")
               (sp-wrap-with-pair ,val)))))

(def-pairs ((paren        . "(")
            (bracket      . "[")
            (brace        . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote   . "`")))

(defun cantsin/smartparens-init ()
  "Initialize smartparens."
  ;; js2 support
  (require 'smartparens)
  (add-to-list 'sp-sexp-suffix (list #'js2-mode 'regexp ""))
  (sp-with-modes '(js2-mode)
    (sp-local-pair "'" "'" :unless '(sp-in-comment-p))))

(defun cantsin/smartparens-config ()
  "Set up smartparens."
  (show-smartparens-global-mode t))

(use-package smartparens-config
  :ensure smartparens
  :bind (("C-M-a" . sp-beginning-of-sexp)
         ("C-M-e" . sp-end-of-sexp)

         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)

         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)

         ("C-<right>" . sp-forward-slurp-sexp)
         ("M-<right>" . sp-forward-barf-sexp)
         ("C-<left>"  . sp-backward-slurp-sexp)
         ("M-<left>"  . sp-backward-barf-sexp)

         ;; ("C-M-t" . sp-transpose-sexp)
         ("C-M-k" . sp-kill-sexp)
         ("C-k"   . sp-kill-hybrid-sexp)
         ("M-k"   . sp-backward-kill-sexp)
         ("C-M-w" . sp-copy-sexp)

         ("M-[" . sp-backward-unwrap-sexp)
         ("M-]" . sp-unwrap-sexp)

         ("C-x C-t" . sp-transpose-hybrid-sexp)

         ("C-c ("  . wrap-with-parens)
         ("C-c ["  . wrap-with-brackets)
         ("C-c {"  . wrap-with-braces)
         ("C-c '"  . wrap-with-single-quotes)
         ("C-c \"" . wrap-with-double-quotes)
         ("C-c _"  . wrap-with-underscores)
         ("C-c `"  . wrap-with-back-quotes))
  :init (cantsin/smartparens-init)
  :config (cantsin/smartparens-config))

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)

(use-package helm-dash
  :config (setq helm-dash-browser-func 'eww))

(use-package swiper
  :bind (("\C-c s" . swiper)))

(use-package pdf-tools
  :defer t
  :config (pdf-tools-install))

(require 'comint)
(add-to-list 'comint-preoutput-filter-functions
             (lambda (output)
               (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))

(provide 'setup-buffer)
;;; setup-buffer.el ends here
