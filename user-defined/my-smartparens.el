
(defun my-smartparens-mode-config ()
  "map slurping and barfing (because the default C-M-<right>/<left> were being capture by the terminal)"
  (local-set-key (kbd "M-<right>") 'sp-backward-barf-sexp)
  (local-set-key (kbd "M-<left>") 'sp-backward-slurp-sexp))

(add-hook 'smartparens-mode-hook 'my-smartparens-mode-config)
