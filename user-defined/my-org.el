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
    (let ((hours (/ number 60))
	  (minutes (% number 60)))
      (concat (format "%d" hours)
	      ":"
	      (format "%02d" minutes)))))

(add-hook 'org-mode-hook 'my-org-mode-config)
