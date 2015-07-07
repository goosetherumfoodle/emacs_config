(global-set-key (kbd "C-o") 'flip)
(defun flip (num)
  "Animates flipping a table."
  (interactive "p")
  (let ((start-point (point))
	(anticipation (or num
			  2)))
    (insert "(°-°) ┬─┬ ")
    (sit-for anticipation)
    (delete-region start-point (point))
    (insert "(╯°□°)╯︵ ┻━┻ ")))


	  
