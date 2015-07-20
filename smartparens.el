;; map slurping and barfing (because the default
;; C-M-<right>/<left> were being capture by the terminal
(define-key smartparens-mode-map (kbd "M-<right>") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "M-<left>") 'sp-backward-slurp-sexp)
