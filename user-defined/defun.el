(defun jump-to-newline ()
    "Move to the end of the current line, then create a newline.
\(Like \"o\" in Vi.\) I'm probably replicating a pre-existing command."
    (interactive)
    (move-end-of-line nil)
    (newline)
    (indent-for-tab-command))

;; from: http://ergoemacs.org/emacs/emacs_byte_compile.html
(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
