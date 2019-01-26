;;; setup-eshell -- Summary
;;; Commentary:
;;; Setup eshell.
;;; Code:
(require 'use-package)

(defun eshell/emacs (&rest args)
  "Open a file in Emacs.  Some habits die hard.  Pass on ARGS."
  (if (null args)
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defun eshell/clear ()
  "Scroll contents of eshell window out of sight, leaving a blank window."
  (interactive)
  (let ((number-newlines (count-lines (window-start) (point))))
    (insert (make-string number-newlines ?\n)))
    (eshell-send-input))

(defun eshell/dired ()
  "Call dired on the current directory."
  (dired (eshell/pwd)))

(defun eshell/sb (&rest args)
  "Switch to given buffer, apply ARGS."
  (funcall 'switch-to-buffer (apply 'eshell-flatten-and-stringify args)))

(defun eshell-view-file (file)
  "A version of `view-file' for FILE which properly respects the eshell prompt."
  (interactive "fView file: ")
  (unless (file-exists-p file) (error "%s does not exist" file))
  (let ((had-a-buf (get-file-buffer file))
        (buffer (find-file-noselect file)))
    (if (eq (with-current-buffer buffer (get major-mode 'mode-class))
            'special)
        (progn
          (switch-to-buffer buffer)
          (message "Not using View mode because the major mode is special"))
      (let ((undo-window (list (window-buffer) (window-start)
                               (+ (window-point)
                                  (length (funcall eshell-prompt-function))))))
        (switch-to-buffer buffer)
        (view-mode-enter (cons (selected-window) (cons nil undo-window))
                         'kill-buffer)))))

(defun eshell/less (&rest args)
  "Invoke `view-file' on ARGS.  \"less +42 foo\" will go to \\
line 42 in the buffer for foo.."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (eshell-view-file file)
          (goto-char (point-min))
          (forward-line (1- line)))
      (eshell-view-file (pop args)))))

(defalias 'eshell/more 'eshell/less)

(defun invoke-bash (command)
  "For these situations where eshell won't work, simply invoke a bash COMMAND."
  (let ((invoke-bash-cmd (concat "bash -c \"" command "\"")))
    (message invoke-bash-cmd)
    (throw 'eshell-replace-command (eshell-parse-command invoke-bash-cmd))))

(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
    (goto-char 0)
    (search-forward "available git commands in")
    (let (commands)
      (while (re-search-forward
	      "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
	      nil t)
	(push (match-string 1) commands)
	(when (match-string 2)
	  (push (match-string 2) commands)))
      (sort commands #'string<))))

(defconst pcmpl-git-commands (pcmpl-git-commands)
  "List of `git' commands.")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
	(push (match-string 1) refs))
      (nreverse refs))))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-git-commands))
   ((pcomplete-match (regexp-opt '("pull" "push")) 1)
    (pcomplete-here (pcmpl-git-remotes)))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (append (pcmpl-git-get-refs "heads")
			     (pcmpl-git-get-refs "tags"))))
   (t
    (while (pcomplete-here (pcomplete-entries))))))

(defun eshell/info (subject)
  "Read the Info manual on SUBJECT."
  (let ((buf (current-buffer)))
    (Info-directory)
    (let ((node-exists (ignore-errors (Info-menu subject))))
      (if node-exists
          0
        ;; We want to switch back to *eshell* if the requested
        ;; Info manual doesn't exist.
        (switch-to-buffer buf)
        (eshell-print (format "There is no Info manual on %s.\n" subject))
        1))))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file.  The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(defun eshell/x ()
  "Quick way to exit."
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(defalias 'eshell/basename 'file-name-nondirectory)

(defun cantsin/eshell-config ()
  "Configure eshell."
  (use-package em-smart)
  (use-package em-prompt)
  (use-package em-dirs)
  (use-package eshell-prompt-extras)

  (load "em-hist") ; So the history vars are defined
  (if (boundp 'eshell-save-history-on-exit)
      (setq eshell-save-history-on-exit t)) ; Don't ask, just save

  (eval-after-load 'esh-opt
    (progn
      (setq eshell-highlight-prompt nil
            eshell-prompt-function 'epe-theme-lambda)))

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t
        eshell-highlight-prompt t
        eshell-prompt-regexp "\$ "))

(use-package eshell
  :defer t
  :bind (("C-!" . eshell-here))
  :config (cantsin/eshell-config))

(provide 'setup-eshell)
;;; setup-eshell.el ends here
