;; clojure/cider mode hooks
(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'show-paren-mode)
(add-hook 'clojure-mode-hook 'smartparens-mode)

(setq nrepl-log-messages t)

;; don't open repl in 2nd frame when cider starts
(setq cider-repl-pop-to-buffer-on-connect nil)
