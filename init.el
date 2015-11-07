;; This file executes a few functions and then loads config.org
;; I'm not tracking the functions at the bottom of the file automatically created by emacs

;; These disable the menu bar and other gui things
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

;; Load to blank buffer
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; copied from MELPA site
;; configure emacs package manager to include melpa packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Require use-package, althought I can't get it working right
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; taken from the use-package docs
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; Finally compile and load the lisp from the org-mode file
(org-babel-load-file (concat user-emacs-directory "config.org"))
