;; This file executes a few functions and then loads config.org
;; I'm not tracking the functions at the bottom of the file automatically created by emacs

;; These disable the menu bar and other gui things
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

;; copied from MELPA site
;; configure emacs package manager to include melpa packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Require use-package, althought I can't get it working right
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; taken from the use-package docs
(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant

;; Finally compile and load the lisp from the org-mode file
(org-babel-load-file (concat user-emacs-directory "config.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; EMACS CONTROLLED VARIABLES :::::::::::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (clj-refactor rainbow-delimiters enh-ruby-mode smartparens use-package undo-tree term-alert sicp restart-emacs request-deferred magit intero helm cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
