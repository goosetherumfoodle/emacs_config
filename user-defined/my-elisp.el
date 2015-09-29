;; emacs-lisp mode
;; (setq emacs-lisp-mode-hook (cons emacs-lisp-mode-hook '(smartparens-mode show-paren-mode eldoc-mode rainbow-delimiters-mode)))
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; automatically byte-compile any .el file that already
;; has an .elc file
(add-hook 'after-save-hook 'byte-compile-current-buffer)