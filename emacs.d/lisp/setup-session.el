;;; setup-session -- Summary
;;; Commentary:
;;; Setup session.
;;; Code:
(require 'use-package)

(defun maybe-reveal ()
  "Reveal contents depending on mode."
  (when (and (or (memq major-mode '(org-mode outline-mode))
                 (and (boundp 'outline-minor-mode)
                      outline-minor-mode))
             (outline-invisible-p))
    (if (eq major-mode 'org-mode)
        (org-reveal)
      (show-subtree))))

(defun cantsin/session-init ()
  "Initialize session variables."
  ;; don't change edit position in the damn commit buffer/file.
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  ;; fix for overflow
  (setq session-save-print-spec '(t nil 40000))

  ;; fix org oddities
  (add-hook 'after-init-hook 'session-initialize)
  (add-hook 'session-after-jump-to-last-change-hook 'maybe-reveal)
  ;;(add-to-list 'session-globals-exclude 'org-mark-ring)
  )

(use-package session
  :ensure t
  :defer t
  :init (cantsin/session-init))

(provide 'setup-session)
;;; setup-session.el ends here
