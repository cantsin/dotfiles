;;; setup-org -- Summary
;;; Commentary:
;;; Setup org-mode.
;;; Code:
(require 'use-package)

(defun update-parent-cookie ()
  "Update the count of items in this section."
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  "When killing the line, update the cookie."
  (update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  "When killing the whole line, also update the cookie."
  (update-parent-cookie))

(defun cantsin/org-init ()
  "Initialize org."
  (use-package org-bullets
    :defer t
    :ensure t)
  (use-package stripe-buffer
    :defer t
    :ensure t)
  (add-hook 'org-mode-hook
            (lambda () (org-bullets-mode 1)))
  (add-hook 'org-after-todo-state-change-hook
            (lambda () (org-update-statistics-cookies t)))
  (add-hook 'org-mode-hook
            'org-table-stripes-enable)
  (add-hook 'org-mode-hook
            #'(lambda () (setq electric-indent-mode nil))))

(defun cantsin/org-config ()
  "Set up org."
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (setq org-default-notes-file "~/todos.org/notes.org"
        org-agenda-files '("~/todos.org/")
        org-journal-dir "~/workspace/journal"
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-startup-indented t
        org-return-follows-link t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-use-fast-todo-selection t
        org-agenda-start-on-weekday 6
        org-archive-location ".archived.org::* From %s"
        org-columns-default-format "%60ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM"
        org-stuck-projects '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:")
        org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
          ("r" "respond" entry (file org-default-notes-file)
           "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n\n"))
        org-todo-keywords
        '((sequence "TODO(t)" "PENDING(p)" "|" "DONE(d)")
          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
          (sequence "|" "FUTURE(l)")
          (sequence "|" "CANCELED(c)"))
  ;; set up shortcut for priorities
        org-agenda-custom-commands
        '(("p" "Agenda for all priorities" agenda ""
           ((org-agenda-skip-function
             '(and
               (not
                (org-entry-get nil "PRIORITY"))
               (point-at-eol)))))))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t))))

(use-package org
  :bind (("C-c l" . org-store-link)
         ("M-p" . org-previous-visible-heading)
         ("M-n" . org-next-visible-heading)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c e" . org-archive-subtree))
  :init (cantsin/org-init)
  :config (cantsin/org-config))

(provide 'setup-org)
;;; setup-org.el ends here
