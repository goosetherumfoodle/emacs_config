(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x g") 'magit-status)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)


(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      eval-expression-print-length nil)

(ido-mode t)
(setq ido-enable-flex-matching t)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))



(defun my-web-mode-config ()
  (setq web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-config)

;; TODO: Fix this
;; First attempt at sorting out config
(add-to-list 'load-path (expand-file-name "~/.emacs.d/load-paths.el"))
;;(require 'load-paths)

(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

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
  (local-set-key (kbd "C-M-k") 'kill-line))

(setq whitespace-style '(face trailing empty))

(add-hook 'enh-ruby-mode-hook 'my-enh-ruby-mode-config)
(add-hook 'prog-mode-hook 'whitespace-mode)

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
;; (setq emacs-lisp-mode-hook (cons emacs-lisp-mode-hook '(smartparens-mode show-paren-mode eldoc-mode rainbow-delimiters-mode)))
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-term-color-vector
   [unspecified "#424242" "#EF9A9A" "#C5E1A5" "#FFEE58" "#64B5F6" "#E1BEE7" "#80DEEA" "#E0E0E0"])
 '(custom-enabled-themes (quote (cyberpunk)))
 '(custom-safe-themes
   (quote
    ("52706f54fd3e769a0895d1786796450081b994378901d9c3fb032d3094788337" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "8f2e60e25bd33a29f45867d99c49afd9d7f3f3ed8a60926d32d5a23c790de240" "c158c2a9f1c5fcf27598d313eec9f9dceadf131ccd10abc6448004b14984767c" "cdd26fa6a8c6706c9009db659d2dffd7f4b0350f9cc94e5df657fa295fffec71" "90e4b4a339776e635a78d398118cb782c87810cb384f1d1223da82b612338046" "7997e0765add4bfcdecb5ac3ee7f64bbb03018fb1ac5597c64ccca8c88b1262f" "0c387e27a3dd040b33c6711ff92e13bd952369a788eee97e4e4ea2335ac5528f" "9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "ad24ea739f229477ea348af968634cb7a0748c9015110a777c8effeddfa920f5" "90edd91338ebfdfcd52ecd4025f1c7f731aced4c9c49ed28cfbebb3a3654840b" "1c50040ec3b3480b1fec3a0e912cac1eb011c27dd16d087d61e72054685de345" "0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" default)))
 '(evil-emacs-state-cursor (quote ("#E57373" bar)))
 '(evil-insert-state-cursor (quote ("#E57373" hbar)))
 '(evil-normal-state-cursor (quote ("#FFEE58" box)))
 '(evil-visual-state-cursor (quote ("#C5E1A5" box)))
 '(fringe-mode 10 nil (fringe))
 '(highlight-symbol-colors
   (quote
    ("#FFEE58" "#C5E1A5" "#80DEEA" "#64B5F6" "#E1BEE7" "#FFCC80")))
 '(highlight-symbol-foreground-color "#E0E0E0")
 '(highlight-tail-colors
   (if
       (eq
	(quote dark)
	(quote light))
       (quote
	(("#FFA726" . 0)
	 ("#FFEE58" . 10)
	 ("#FFF59D" . 30)
	 ("#494949" . 60)
	 ("#424242" . 80)))
     (quote
      (("#F8BBD0" . 0)
       ("#FF80AB" . 10)
       ("#9575CD" . 30)
       ("#494949" . 60)
       ("#424242" . 80)))))
 '(linum-format " %6d ")
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(package-selected-packages
   (quote
    (magit hc-zenburn-theme zenburn-theme guru-mode hackernews afternoon-theme ample-theme ample-zen-theme anti-zenburn-theme apropospriate-theme atom-dark-theme badger-theme bliss-theme bubbleberry-theme calmer-forest-theme clues-theme colonoscopy-theme dakrone-theme darkburn-theme darkmine-theme darktooth-theme distinguished-theme faff-theme farmhouse-theme firebelly-theme flatui-theme grandshell-theme green-phosphor-theme gruvbox-theme zerodark-theme cyberpunk-theme yasnippet yaml-mode web-mode smartparens scss-mode sass-mode ruby-interpolation ruby-hash-syntax ruby-electric rubocop rspec-mode rotate rinari regex-tool rainbow-delimiters projectile-rails paxedit multi-term markdown-mode lush-theme hipster-theme gitignore-mode flymake-yaml flymake-shell flymake-sass flymake-ruby flycheck-clojure feature-mode evil erefactor enh-ruby-mode clojure-cheatsheet autopair adjust-parens ac-cider 4clojure)))
 '(pos-tip-background-color "#3a3a3a")
 '(pos-tip-foreground-color "#9E9E9E")
 '(powerline-color1 "#222232")
 '(powerline-color2 "#333343")
 '(sp-base-key-bindings (quote sp))
 '(tabbar-background-color "#353535")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#d01A4E")
     (60 . "#cb4b16")
     (80 . "#b58900")
     (100 . "#b58900")
     (120 . "#b58900")
     (140 . "#7E7D7E")
     (160 . "#7E7D7E")
     (180 . "#9FAA9B")
     (200 . "#9FC59F")
     (220 . "#859900")
     (240 . "#31be67")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#268bd2")
     (320 . "#268bd2")
     (340 . "#00a74e")
     (360 . "#d33682"))))
 '(vc-annotate-very-old-color "#d33682")
 '(when
      (or
       (not
	(boundp
	 (quote ansi-term-color-vector)))
       (not
	(facep
	 (aref ansi-term-color-vector 0))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sp-pair-overlay-face ((t nil))))
