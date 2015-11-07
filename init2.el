(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; from MELPA site
;; configure emacs package manager to include melpa packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;;For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(org-babel-load-file (concat user-emacs-directory "config.org"))

;;;;;;;;;;;;;;;;;;;;
;;; set by emacs ;;;
;;;;;;;;;;;;;;;;;;;;
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(ansi-term-color-vector
   [unspecified "#424242" "#EF9A9A" "#C5E1A5" "#FFEE58" "#64B5F6" "#E1BEE7" "#80DEEA" "#E0E0E0"] t)
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (cyberpunk)))
 '(custom-safe-themes
   (quote
    ("8fed5e4b89cf69107d524c4b91b4a4c35bcf1b3563d5f306608f0c48f580fdf8" "83e584d74b0faea99a414a06dae12f11cd3176fdd4eba6674422539951bcfaa8" "7abf5a28ec511e7e8f5fe10978b3d63058bbd280ed2b8d513f9dd8b7f5fc9400" "ba9be9caf9aa91eb34cf11ad9e8c61e54db68d2d474f99a52ba7e87097fa27f5" "ac5584b12254623419499c3a7a5388031a29be85a15fdef9b94df2292d3e2cbb" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "52706f54fd3e769a0895d1786796450081b994378901d9c3fb032d3094788337" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "8f2e60e25bd33a29f45867d99c49afd9d7f3f3ed8a60926d32d5a23c790de240" "c158c2a9f1c5fcf27598d313eec9f9dceadf131ccd10abc6448004b14984767c" "cdd26fa6a8c6706c9009db659d2dffd7f4b0350f9cc94e5df657fa295fffec71" "90e4b4a339776e635a78d398118cb782c87810cb384f1d1223da82b612338046" "7997e0765add4bfcdecb5ac3ee7f64bbb03018fb1ac5597c64ccca8c88b1262f" "0c387e27a3dd040b33c6711ff92e13bd952369a788eee97e4e4ea2335ac5528f" "9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "ad24ea739f229477ea348af968634cb7a0748c9015110a777c8effeddfa920f5" "90edd91338ebfdfcd52ecd4025f1c7f731aced4c9c49ed28cfbebb3a3654840b" "1c50040ec3b3480b1fec3a0e912cac1eb011c27dd16d087d61e72054685de345" "0820d191ae80dcadc1802b3499f84c07a09803f2cb90b343678bdb03d225b26b" default)))
 '(evil-emacs-state-cursor (quote ("#E57373" bar)) t)
 '(evil-insert-state-cursor (quote ("#E57373" hbar)) t)
 '(evil-normal-state-cursor (quote ("#FFEE58" box)) t)
 '(evil-visual-state-cursor (quote ("#C5E1A5" box)) t)
 '(fci-rule-color "#383838")
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
 '(org-agenda-files (quote ("~/Documents/org/org.org")))
 '(package-selected-packages
   (quote
    (flymake-coffee roguel-ike coffee-mode restart-emacs elixir-mode flymake-haml arduino-mode auctex company geiser magit hc-zenburn-theme zenburn-theme guru-mode hackernews afternoon-theme ample-theme ample-zen-theme anti-zenburn-theme apropospriate-theme atom-dark-theme badger-theme bliss-theme bubbleberry-theme calmer-forest-theme clues-theme colonoscopy-theme dakrone-theme darkburn-theme darkmine-theme darktooth-theme distinguished-theme faff-theme farmhouse-theme firebelly-theme flatui-theme grandshell-theme green-phosphor-theme gruvbox-theme zerodark-theme cyberpunk-theme yasnippet yaml-mode web-mode smartparens scss-mode sass-mode ruby-interpolation ruby-hash-syntax ruby-electric rubocop rspec-mode rotate rinari regex-tool rainbow-delimiters projectile-rails paxedit multi-term markdown-mode lush-theme hipster-theme gitignore-mode flymake-yaml flymake-shell flymake-sass flymake-ruby flycheck-clojure feature-mode evil erefactor enh-ruby-mode clojure-cheatsheet autopair adjust-parens ac-cider 4clojure)))
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
