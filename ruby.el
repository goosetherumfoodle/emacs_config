(add-to-list
 'auto-mode-alist
 '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(defun my-enh-ruby-mode-config ()
  ;; (local-set-key (kbd "C-k") 'previous-line)
  ;; (local-set-key (kbd "C-j") 'next-line)
  ;; (local-set-key (kbd "C-h") 'backward-char)
  ;; (local-set-key (kbd "C-l") 'forward-char)
  ;; (local-set-key (kbd "M-h") 'backward-word)
  ;; (local-set-key (kbd "M-l") 'forward-word)
  (local-set-key (kbd "C-o") 'jump-to-newline)
  ;; (local-set-key (kbd "C-M-h") 'help)
  ;; (local-set-key (kbd "C-M-l") 'recenter-top-bottom)
  ;; (local-set-key (kbd "C-M-k") 'kill-line)
  )

(add-hook 'enh-ruby-mode-hook 'my-enh-ruby-mode-config)
;; ruby-mode
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
