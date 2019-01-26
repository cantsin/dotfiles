;;; setup-company -- Summary
;;; Commentary:
;;; Setup auto completion.
;;; Code:
(require 'use-package)

(defun cantsin/setup-company ()
  "Set up company."
  (global-company-mode t)
  (define-key company-active-map (kbd "C-n")
    (lambda () (interactive) (company-complete-common-or-cycle 1)))
  (define-key company-active-map (kbd "C-p")
    (lambda () (interactive) (company-complete-common-or-cycle -1)))
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 1.5
        company-minimum-prefix-length 2
        company-show-numbers t
        company-selection-wrap-around t
        company-dabbrev-ignore-case t
        company-dabbrev-ignore-invisible t
        company-dabbrev-downcase nil
        company-backends (list #'company-css
                               #'company-clang
                               #'company-capf
                               (list #'company-dabbrev-code
                                     #'company-keywords)
                               #'company-files
                               #'company-dabbrev)))

(use-package company
  :defer t
  :diminish " co"
  :config (cantsin/setup-company))

(provide 'setup-company)
;;; setup-company.el ends here
