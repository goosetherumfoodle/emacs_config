(setq major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq whitespace-style '(trailing empty indentation))

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list
 'auto-mode-alist
 '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;; from MELPA site
;; configure emacs package manager to include melpa packages
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;;For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Set auto-save file location to ~/.emacs.d/auto-save/
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t))) 

(defun my-enh-ruby-mode-config ()
  (local-set-key (kbd "C-k") 'previous-line)
  (local-set-key (kbd "C-j") 'next-line)
  (local-set-key (kbd "C-h") 'backward-char)
  (local-set-key (kbd "C-l") 'forward-char)
  (local-set-key (kbd "M-h") 'backward-word)
  (local-set-key (kbd "M-l") 'forward-word)
  (local-set-key (kbd "C-o") 'jump-to-newline)
  (local-set-key (kbd "C-M-h") 'help)
  (local-set-key (kbd "C-M-l") 'recenter-top-bottom)
  (local-set-key (kbd "C-M-k") 'kill-line)
  (setq whitespace-style '(face trailing empty indentation))
  (require whitespace-mode))

(add-hook 'enh-ruby-mode-hook 'my-enh-ruby-mode-config)

;; remap movement keys to be vim-like
(global-set-key (kbd "C-k") 'previous-line)
(global-set-key (kbd "C-j") 'next-line)
(global-set-key (kbd "C-h") 'backward-char)
(global-set-key (kbd "C-l") 'forward-char)
(global-set-key (kbd "M-h") 'backward-word)
(global-set-key (kbd "M-l") 'forward-word)
(global-set-key (kbd "C-o") 'jump-to-newline)

;; remap keys overridden above
(global-set-key (kbd "C-M-h") 'help)
(global-set-key (kbd "C-M-l") 'recenter-top-bottom)
(global-set-key (kbd "C-M-k") 'kill-line)

;; set hippie-exand
(global-set-key (kbd "M-/") 'hippie-expand)

;; set cycle-spacing
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; ruby-mode 
(add-hook 'ruby-mode-hook 'ruby-electric-mode)

;; emacs-lisp mode
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'autopair-mode)

;; clojure/cider mode hooks
(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'show-paren-mode)
(add-hook 'clojure-mode-hook 'smartparens-mode)
(setq nrepl-log-messages t)

;; don't open repl in 2nd frame when cider starts
(setq cider-repl-pop-to-buffer-on-connect nil)

;; map slurping and barfing for smartparens mode (because the default
;; C-M-<right>/<left> were being capture by the terminal
(define-key smartparens-mode-map (kbd "M-<right>") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "M-<left>") 'sp-backward-slurp-sexp)

;; from https://github.com/pezra/rspec-mode
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(ad-activate 'rspec-compile)

;;; Custom functions
(defun jump-to-newline ()
    "Move to the end of the current line, then create a newline.
\(Like \"o\" in Vi.\) I'm probably replicating a pre-existing command."
    (interactive)
    (move-end-of-line nil)
    (newline)
    (indent-for-tab-command))

;; (╯°□°)╯︵ ┻━┻ 
(defun flip-table (num)
  "Animates flipping a table."
  (interactive "p")
  (let ((start-point (point))
	(anticipation (or num 4)))
    (insert "(°-°) ┬─┬ ")
    (sit-for anticipation)
    (delete-region start-point (point))
    (insert "(╯°□°)╯︵ ┻━┻ ")))

;; flip-pɹoʍ︵\(°□°\)  
(defun flip-word (num)
  "Animates flipping the last word."
  (interactive "p")
    (let ((anticipation (or num 4)))
      (re-search-backward "\\(\\<\\w+\\>[.,!?]?\\)")
      (goto-char (match-end 0))
      (insert " (°-°)")
      (let ((post-face (point)))
	(sit-for anticipation)
	(replace-match (rotate-word (match-string-no-properties 0)))
	(delete-region (match-end 0) post-face))
      (insert "︵\\(°□°\\) ")))

;; used in flip-word
(defun rotate-word (string)
  (let ((flipped))
    (dolist (ascii-dec (string-to-list string))
      (setq flipped (cons 
		     (unicode-to-char
		      (dec-to-upside-down-unicode ascii-dec))
		     flipped)))
    (concat flipped)))

;; used in rotate-word
(defun unicode-to-char (unicode)
  (string-to-number unicode 16))

;; used in rotate-word
(defun dec-to-upside-down-unicode (dec)
  (cond ((= dec 97) "0250")
	((= dec 98) "0071")
	((= dec 99) "0254")
	((= dec 100) "0070")
	((= dec 101) "01dd")
	((= dec 102) "025f")
	((= dec 103) "0253")
	((= dec 104) "0265")
	((= dec 105) "0131")
	((= dec 106) "027e")
	((= dec 107) "029e")
	((= dec 108) "006c")
	((= dec 109) "026f")
	((= dec 110) "0075")
	((= dec 111) "006f")
	((= dec 112) "0064")
	((= dec 113) "0062")
	((= dec 114) "0279")
	((= dec 115) "0073")
	((= dec 116) "0287")
	((= dec 117) "006e")
	((= dec 118) "028c")
	((= dec 119) "028d")
	((= dec 120) "0078")
	((= dec 121) "028e")
	((= dec 122) "007a")
	((= dec 65) "2200")
	((= dec 66) "10412")
	((= dec 67) "0186")
	((= dec 68) "15e1")
	((= dec 69) "018e")
	((= dec 70) "2132")
	((= dec 71) "2141")
	((= dec 72) "0048")
	((= dec 73) "0049")
	((= dec 74) "017f")
	((= dec 75) "029e")
	((= dec 76) "2142")
	((= dec 77) "0057")
	((= dec 78) "004e")
	((= dec 79) "004f")
	((= dec 80) "0500")
	((= dec 81) "038c")
	((= dec 82) "1d1a")
	((= dec 83) "0053")
	((= dec 84) "22a5")
	((= dec 85) "2229")
	((= dec 86) "039b")
	((= dec 87) "004d")
	((= dec 88) "0058")
	((= dec 89) "2144")
	((= dec 90) "005a")
	((= dec 48) "0030")
	((= dec 49) "21c2")
	((= dec 50) "218a")
	((= dec 51) "218b")
	((= dec 52) "3123")
	((= dec 53) "078e")
	((= dec 54) "0039")
	((= dec 55) "3125")
	((= dec 56) "0038")
	((= dec 57) "0036")
	((= dec 38) "214b")
	((= dec 45) "203e")
	((= dec 63) "00bf")
	((= dec 33) "00a1")
	((= dec 34) "201e")
	((= dec 39) "002c")
	((= dec 46) "02d9")
	((= dec 44) "0027")
	((= dec 59) "061b")
	(t nil)))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sp-base-key-bindings (quote sp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sp-pair-overlay-face ((t nil))))
