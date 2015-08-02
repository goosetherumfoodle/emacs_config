(add-to-list
 'auto-mode-alist
 '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(defun my-enh-ruby-mode-config ()
  (local-set-key (kbd "C-o") 'jump-to-newline))

(add-hook 'enh-ruby-mode-hook 'my-enh-ruby-mode-config)
;; ruby-mode
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
