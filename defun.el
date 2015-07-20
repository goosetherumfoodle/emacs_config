(defun jump-to-newline ()
    "Move to the end of the current line, then create a newline.
\(Like \"o\" in Vi.\) I'm probably replicating a pre-existing command."
    (interactive)
    (move-end-of-line nil)
    (newline)
    (indent-for-tab-command))
