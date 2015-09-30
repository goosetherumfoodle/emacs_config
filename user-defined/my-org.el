(defun my-org-mode-config ()
  (local-set-key (kbd "C-M-j") 'org-insert-heading)
  (local-set-key (kbd "C-<RET>") 'org-insert-heading-respect-content)

  (defun to-minutes (time-string)
    "Accepts a string of format '(h)h:mm' and returns total minutes"
    (string-match "\\([0-9]+\\):\\([0-9]\\{2,\\}\\)" time-string)
    (let ((hours (string-to-number (match-string 1 time-string)))
	  (minutes (string-to-number (match-string 2 time-string))))
      (if (> minutes 59)
	  (error (concat (number-to-string minutes) " is not between 0 and 59"))
	(+ minutes (* hours 60)))))

  (defun number-to-time (number)
    "Converts a float into formatted string (hh:mm)"
    (let ((hours (/ number 60))
	  (minutes (% number 60)))
      (concat (format "%d" hours)
	      ":"
	      (format "%02d" minutes))))

  (defun sum-times (time-list)
    "Takes a list of times (hh:mm), and returns sum in the same format (hh:mm)"
    (number-to-time (apply '+ (mapcar 'to-minutes time-list))))

  (defun time-to-wage (time dollars-per-hour)
    "Converts time (hh:mm) to wages."
    (let ((minutes (to-minutes time)))
      (let ((hours (/ minutes
		      60.0)))
	(format "%0.2f" (* hours dollars-per-hour)))))

  (defun number-to-dollars (float)
    "Formats float into dollar string"
    (format "$%0.2f" float)))

(add-hook 'org-mode-hook 'my-org-mode-config)
