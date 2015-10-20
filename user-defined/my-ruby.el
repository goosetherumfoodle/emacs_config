(add-to-list
 'auto-mode-alist
 '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(defun my-enh-ruby-mode-config ()
  (local-set-key (kbd "C-o") 'jump-to-newline)
  (fset 'insert-pry
	(lambda (&optional arg)
	  "Keyboard macro."
	  (interactive "p")
	  (kmacro-exec-ring-item
	   (quote ("require 'pry'; binding.pry" 0 "%d"))
	   arg))))

(add-hook 'enh-ruby-mode-hook 'my-enh-ruby-mode-config)
;; ruby-mode
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
