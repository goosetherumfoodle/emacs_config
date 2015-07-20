(defun my-org-mode-config ()
  (local-set-key (kbd "C-M-j") 'org-insert-heading)
  (local-set-key (kbd "C-<RET>") 'org-insert-heading-respect-content))

(add-hook 'org-mode-hook 'my-org-mode-config)
