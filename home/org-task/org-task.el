;;; package --- Summary

;;; Commentary:

;; print out the current org mode task, if any, along with the elapsed
;; time and estimated time.

;;; Code:
(provide 'current-task)

(require 'org)
(require 'org-clock)

(defun org-clock-get-clocked-time ()
  "Get the clocked time for the current item in minutes.
The time returned includes the time spent on this task in
previous clocking intervals."
  (let ((currently-clocked-time
	 (floor (- (float-time)
		   (float-time org-clock-start-time)) 60)))
    (+ currently-clocked-time (or org-clock-total-time 0))))

(defun org-elapsed ()
  "Format the elapsed time."
  (let* ((elapsed (org-clock-get-clocked-time))
         (hours (/ elapsed 60))
         (minutes (% elapsed 60)))
    (concat (number-to-string hours) ":" (format "%02d" minutes))))

(defun get-current-task ()
  "Get the current clocked org task, if any."
  (if (and (boundp 'org-clock-current-task) org-clock-current-task)
      (let ((elapsed (org-elapsed))
            (task-name (org-no-properties org-clock-current-task))
            (estimated org-clock-effort))
        (concat task-name " [" elapsed "/" estimated "]"))
    "(none)"))
;;; current-task ends here
