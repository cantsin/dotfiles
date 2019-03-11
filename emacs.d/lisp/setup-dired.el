;;; setup-dired -- Summary
;;; Commentary:
;;; Setup dired.
;;; Code:
(require 'use-package)

(defun dired-back-to-top ()
  "Skip the . and .. directories."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 4))

(defun dired-jump-to-bottom ()
  "Skip the blank line at the end of dired."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(defun cantsin/dired-config ()
  "Set up dired."
  (use-package stripe-buffer
    :defer t
    :ensure t
    :config (progn
              (set-face-background 'stripe-hl-line "dark violet")
              (set-face-foreground 'stripe-hl-line "white")))
  (add-hook 'dired-mode-hook 'stripe-listify-buffer)
  (define-key dired-mode-map
    [remap beginning-of-buffer] 'dired-back-to-top)
  (define-key dired-mode-map
    [remap end-of-buffer] 'dired-jump-to-bottom)

  ;; Auto refresh dired, but be quiet about it
  (use-package autorevert
    :defer t)
  (setq dired-listing-switches "-alhv"
        dired-dwim-target t
        dired-clean-up-buffers-too t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package dired
  :defer t
  :config (cantsin/dired-config))

(require 'dired-x)

(provide 'setup-dired)
;;; setup-dired.el ends here
