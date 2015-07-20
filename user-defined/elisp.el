;; emacs-lisp mode
;; (setq emacs-lisp-mode-hook (cons emacs-lisp-mode-hook '(smartparens-mode show-paren-mode eldoc-mode rainbow-delimiters-mode)))
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; from: http://ergoemacs.org/emacs/emacs_byte_compile.html
(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

