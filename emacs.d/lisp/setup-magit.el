;;; setup-magit -- Summary
;;; Commentary:
;;; Setup magit.
;;; Code:
(require 'use-package)

(defun cantsin/magit-init ()
  "Set up magit properly."
  (progn
    ;; we no longer need vc-git
    (delete 'Git vc-handled-backends)))

(defun magit-toggle-whitespace ()
  "Toggle whitespace."
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  "Ignore whitespace."
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  "Do not ignore whitespace."
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(defun visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/compare/%s"
     (replace-regexp-in-string
      "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
      (magit-get "remote"
                 (magit-get-current-remote)
                 "url"))
     (magit-get-current-branch))))

(defun cantsin/magit-config ()
  "Configure magit appropriately."
  (progn
    (if (eq system-type 'windows-nt)
        (setq magit-git-executable "C:\\Program Files (x86)\\Git\\bin\\git.exe"))

    (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

    ;; magit sometimes does not return to the previous buffer correctly
    (setq previous-buffer-under-magit nil)
    (defadvice magit-mode-display-buffer (before cache-buffer-behind-magit activate)
      "Set previous buffer."
      (when (not (string-prefix-p "*magit" (buffer-name)))
        (setq previous-buffer-under-magit (current-buffer))))
    (defadvice magit-mode-quit-window (after restore-buffer-behind-magit activate)
      "Switch to previous buffer."
      (when previous-buffer-under-magit
        (switch-to-buffer previous-buffer-under-magit)
        (setq previous-buffer-under-magit nil)))

    (add-hook 'global-git-commit-mode-hook
              '(lambda ()
                 (progn
                   (beginning-of-buffer)
                   (end-of-line)))
              t)

    ;; magit settings
    (set-face-foreground 'diff-context "#666666")
    (set-face-foreground 'diff-added "#00cc33")
    (set-face-foreground 'diff-removed "#ff0000")

    (setq magit-last-seen-setup-instructions "1.4.0"
          magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
          magit-status-buffer-switch-function 'switch-to-buffer
          magit-diff-refine-hunk t
          magit-rewrite-inclusive 'ask
          magit-process-popup-time 10
          magit-set-upstream-on-push 'askifnotset)))

(use-package magit
  :ensure t
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-file-log))
  :init (cantsin/magit-init)
  :config (cantsin/magit-config))

(use-package ediff
  :defer t
  :config (setq diff-switches "-u"
                ediff-diff-options "-w"
                ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-window-setup-function 'ediff-setup-windows-plain))

(provide 'setup-magit)
;;; setup-magit.el ends here
