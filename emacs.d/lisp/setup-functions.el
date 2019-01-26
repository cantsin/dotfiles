;;; setup-functions -- Summary
;;; Commentary:
;;; Setup various bits that don't really go anywhere else.
;;; Code:

(defadvice pop-to-mark-command (around ensure-new-position activate)
  "When popping the mark, continue popping until the cursor will move \\
Also, if the last command was a copy - skip past all the expand-region cruft."
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(defun url-decode-region (start end)
  "URL decode a region from START to END."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(require 'epg)
(defun epg--check-error-for-decrypt (context)
  "CONTEXT is an epg context.
Here, we work around a bug in epg where gpg prints out \"Good
signature\" and is yet otherwise successful."
  (let ((errors (epg-context-result-for context 'error)))
    (if (epg-context-result-for context 'decryption-failed)
	(signal 'epg-error
		(list "Decryption failed" (epg-errors-to-string errors))))
    (unless (or (epg-context-result-for context 'decryption-okay)
                (equal errors '((exit))))
      (signal 'epg-error
	      (list "Can't decrypt" (epg-errors-to-string errors))))))

(provide 'setup-functions)
;;; setup-functions.el ends here
