In the startup process emacs will read from [init.el](init.el) which will do
some initial setup, [tangle](https://en.wikipedia.org/wiki/Literate_programming) the source blocks in [config.org](config.md) to generate
and load `config.el` (which isn't tracked in git).

When viewing this in emacs, to learn about a function put the point
over it and hit `C-h f <ENTER>`. To learn about a variable put the
cursor over it and hit `C-h v <ENTER>`. Source-blocks can be folded by
putting the point over them and hitting `<TAB>`.


# Table of Contents

1.  [notes](#org25f32a6)
    1.  [org mode](#orga08fa18)
    2.  [literate programming](#org8a8c128)
    3.  [dotenv files](#orgbba4c26)
    4.  [emacs package management](#org37685f8)
2.  [configuration](#org5ef36c3)
    1.  [package installation](#org8a18fca)
    2.  [intial](#orga1f7ba3)
    3.  [macOS](#orgae81a4c)
    4.  [global keybindings](#orgf7ce521)
    5.  [buffer wrangling](#org71a769c)
    6.  [text-mode](#orgda3feba)
    7.  [prog-mode](#org4f4eff0)
    8.  [org-mode](#org521adc1)
    9.  [ttl-mode](#orgc4ddd44)
    10. [projectile](#org7cdf264)
    11. [smart-parens-mode](#org0890d7a)
    12. [web-mode](#org867f6d2)
    13. [whitespace-mode](#org536bbfc)
    14. [dired-mode](#org5f3fa74)
    15. [config for Ruby](#orgf815a82)
    16. [config for Clojure](#org2b291dd)
    17. [config for emacs lisp](#org32adfb6)
    18. [config for scheme](#org39677a0)
    19. [config for coffeescript](#org0e696fc)
    20. [purescript](#org1a3bca6)
    21. [language server protocol](#orga28e1ed)
    22. [term-alert](#org1860d84)
    23. [javascript](#org33d4496)
    24. [haskell](#org3c2c3ac)
    25. [Scala](#org3bfa86b)
    26. [elm](#org879ad67)
    27. [java](#org951f57d)
    28. [python](#orgf557e35)
    29. [json](#org1da0c35)
    30. [git](#org049c67e)
    31. [RSS](#org40b2df8)
    32. [eshell](#orgdcb1356)
    33. [Twilio sms](#orgf633e5f)
    34. [custom functions](#org72088d9)
    35. [novelty functions](#org07e5f2e)


<a id="org25f32a6"></a>

# notes


<a id="orga08fa18"></a>

## org mode


### resources

[org-contrib](https://orgmode.org/worg/org-contrib/) lists some  more org mode funtionality that might be worth checking out.


### inserting source blocks

In the older versions of org-mode you could type `<s` followed by TAB
to insert a code block. Now you need to bring up a menu of templates
to insert with `C-c C-,`


<a id="org8a8c128"></a>

## literate programming

This config file is really only scraping  the surface. [This blog
  post](http://www.howardism.org/Technical/Emacs/literate-programming-tutorial.html) gets into some more org-mode literate programming that I haven't explored yet.


<a id="orgbba4c26"></a>

## dotenv files

I think I must have had something like [dotenv.el](https://github.com/pkulev/dotenv.el) set up in the past
(based on the `env/` functions in the twilio helper functions) but I
evidently didn't save it in my emacs config. so I need to figure that
out again.


<a id="org37685f8"></a>

## emacs package management

Now that I've gotten into nix I'd like to improve my package
management for emacs. That dotenv.el repo contains an interesting
usage of [quelpa-use-package](https://github.com/quelpa/quelpa-use-package) which in turn depends on [quelpa](https://github.com/jwiegley/use-package#package-installation). Instead
of going down a rabbit hole on this right now I need to set aside time
at some point to look into a better emacs package mgmt solution.


<a id="org5ef36c3"></a>

# configuration


<a id="org8a18fca"></a>

## package installation

Bootstrap [use-package](https://github.com/jwiegley/use-package#key-binding) since it'll be used from here on out.

    (require 'use-package)

Make `use-package` default to intstalling the package if it doens't
find it.

    (require 'use-package-ensure)
    (setq use-package-always-ensure t)

Auto-update packages, delete the old versions, and don't spam a
startup buffer with the results.

    (use-package auto-package-update
      :config
      (setq auto-package-update-delete-old-versions t)
      (setq auto-package-update-hide-results t)
      (auto-package-update-maybe))


<a id="orga1f7ba3"></a>

## intial

Use dark theme

    (load-theme 'tango-dark)

Make org-mode the default file mode

    (setq-default major-mode 'org-mode)

This will remove any trailing whitespace on save

    (add-hook 'before-save-hook 'delete-trailing-whitespace)

Let the yank command access the last thing copied to the system
clipboard

    (setq select-enable-clipboard t
          select-enable-primary t
          save-interprogram-paste-before-kill t)

Let the apropos help search include non-interactive functions in its
search

    (setq apropos-do-all t)

Flash top and bottom bars of the screen as an alert

    (setq visible-bell t)

Set the maximum length of a string printed by evaluating a function to
unlimited. (I had to set this to fix some annoying behavior when
evaluating elisp functions).

    (setq eval-expression-print-length nil)

Use ido-mode for finding files and buffers. Enabling flex matching
will make it a fuzzy search.

    (ido-mode t)
    (setq ido-enable-flex-matching t)

Auto-saved files should be saved to '~/.emacs.d/auto-save/'. This
obviously requires this directory to be in place.

    (setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save/") t)))

set eww as the default browser to open links with

    (setq browse-url-browser-function 'eww-browse-url)

remap goto-line `M-g M-g` to preview the line first

    (use-package "goto-line-preview")
    (global-set-key [remap goto-line] 'goto-line-preview)

display column numbers alongside line numbers

    (setq column-number-mode t)


### misc. required packages

packages to include but which there's not much to say about them and
so they are just required in bulk here.

    (use-package "use-package")
    (use-package "dhall-mode")
    (use-package "simple-httpd")
    (use-package "yaml-mode")
    (use-package "restart-emacs")
    (use-package "sicp")
    (use-package "request-deferred")
    (use-package "undo-tree")
    (use-package "cider")
    (use-package "helm-ag")
    (use-package "mustache-mode")
    (use-package "groovy-mode")
    (use-package "leerzeichen")   ; a better whitespace mode
    (use-package "bug-hunter")
    (use-package "sparql-mode")
    (use-package "yaml-mode")
    (use-package "markdown-mode")
    (use-package "racket-mode")
    (use-package "elm-mode")
    (use-package "docker-tramp")
    (use-package "helm-tramp")
    (use-package "shen-mode")
    (use-package "harvest")
    (use-package "rspec-mode")
    (use-package "haml-mode")
    (use-package "rjsx-mode")
    (use-package "web-mode")
    (use-package "slime-volleyball")
    (use-package "clj-refactor")
    (use-package "rainbow-delimiters")
    (use-package "enh-ruby-mode")
    (use-package "smartparens")
    (use-package "magit")
    (use-package "helm")
    (use-package "cider")
    (use-package "undo-tree")
    (use-package "request-deferred")
    (use-package "sicp")
    (use-package "restart-emacs")

Expired packages:

    (use-package "column-marker")
    (use-package "markdown-mode+")


<a id="orgae81a4c"></a>

## macOS

for macbooks, use the command-key as meta, and the option-key as ctrl

    (when (eq system-type 'darwin)
       (setq mac-option-modifier 'control)
       (setq mac-command-modifier 'meta))

    (when (eq system-type 'darwin)
      (setq helm-grep-ag-command "ag --line-numbers -S --hidden --color --color-match '31;43' --nogroup %s %s %s")
      (setq helm-grep-ag-pipe-cmd-switches '("--color-match '31;43'")))

macos has a problem that where the $PATH for gui emacs doens't match
the shell. with any luck this will fix that

    (when (memq window-system '(mac ns x))
      (and
       (not (use-package "exec-path-from-shell"))
       (exec-path-from-shell-initialize)))


<a id="orgf7ce521"></a>

## global keybindings

Use helm for function search

    (use-package "helm")
    (global-set-key (kbd "M-x") 'helm-M-x)

I missed Vi's "o" button which would jump to the next line without
breaking the current line. So I wrote a function to do that and mapped
it to C-o. The function is defined in the custom functions section.

    (global-set-key (kbd "C-o") 'jump-to-newline)

Use hippie-expand instead of the default expand. It will rotate
through many different expansion options. Can be a little much at
times.

    (global-set-key (kbd "M-/") 'hippie-expand)

Set M-SPC to cyle through spaces between characters (alternating
between no space, one space, and however many spaces it is
currently). This can be used as a quick way to delete extra whitespace

    (global-set-key (kbd "M-SPC") 'cycle-spacing)

Sets M-m to jump to a char. Hitting the char mulitple times will keep
moving point to the next. C-M-m jumps backwards.

    (global-set-key (kbd "M-m") 'iy-go-to-char)
    (global-set-key (kbd "C-M-m") 'iy-go-to-char-backward)

Use C-, to switch windows.

    (global-set-key (kbd "C-,") 'other-window)

Use "helpful" menus instead of builtin help menus. These
lines copied from <https://github.com/Wilfred/helpful/blob/0aa289e7a954df456793e7bc1f4bdc3d072e783f/README.md>

    ;; Lookup the current symbol at point. C-c C-d is a common keybinding
    ;; for this in lisp modes.
    (global-set-key (kbd "C-c C-d") #'helpful-at-point)

    ;; Look up *F*unctions (excludes macros).
    ;;
    ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
    ;; already links to the manual, if a function is referenced there.
    (global-set-key (kbd "C-h F") #'helpful-function)

    ;; Look up *C*ommands.
    ;;
    ;; By default, C-h C is bound to describe `describe-coding-system'. I
    ;; don't find this very useful, but it's frequently useful to only
    ;; look at interactive functions.
    (global-set-key (kbd "C-h C") #'helpful-command)


<a id="org71a769c"></a>

## buffer wrangling

With the swap buffers package you can use `M-x swap-buffers` to move
the current buffer to a new window.

    (use-package "swap-buffers")

since I'm using `C-,` for switching windows, I'll use `C-M-,` for
swapping them.

    (global-set-key (kbd "C-M-,") 'swap-buffers)


<a id="orgda3feba"></a>

## text-mode

Turn on auto-fill mode.

    (add-hook 'text-mode-hook 'turn-on-auto-fill)


<a id="org4f4eff0"></a>

## prog-mode

prog mode is the super class of every programming language mode

don't allow indenting to insert tabs

    (add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))

require the linum-relative package which enables relative line
numbers. I've disabled this out because use-package is erroring out.

    (use-package linum-relative)

turn on absolute line numbers. (I'm still working out the kinks with
relative-line numbers)

    (add-hook 'prog-mode-hook (lambda () (linum-mode)))

require a final \n character at the end of the file

    (add-hook 'prog-mode-hook (lambda ()
    			    (setq require-final-newline t)))

Highlight characters in the 80th column in red. Shadows the font-face
color set in the package.

    (use-package "column-marker")
    (add-hook 'prog-mode-hook (lambda ()
    			    (defface column-marker-1 '((t (:background "red")))
      "Face used for a column marker.  Usually a background color."
    			    :group 'faces)
    			    (column-marker-1 80)))

undo-tree mode makes it easier to navigate the undo history as a tree
structure.

    (use-package "undo-tree")
    (add-hook 'prog-mode-hook 'undo-tree-mode)


<a id="org521adc1"></a>

## org-mode

ensure that the C-, doesn't get over-written by org-mode

    (add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-,") 'other-window)))

Set keys that were supposed to be already set in org-mode, but for
some reason were not set for me by default.

    (defun my-org-mode-config ()
      (local-set-key (kbd "C-M-j") 'org-insert-heading)
      (local-set-key (kbd "C-<RET>") 'org-insert-heading-respect-content))
    (add-hook 'org-mode-hook 'my-org-mode-config)

Specify which languages org-mode can execute (by C-c C-c'ing with the
cursor over a code-block). Org-mode can execute many languages, but it
only can execute emacs lisp by defualt, and the rest must be
explicitly enabled. At the moment this only explicitly enables shell
(bash) and ruby.

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
       (ruby . t)))

Disable the warnings that prompt you when you're running elisp
source-blocks within org-mode

    (defun my-org-confirm-babel-evaluate (lang body)
      (not (string= lang "emacs-lisp")))  ; don't query for elisp evaluation
    (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

undo-tree mode makes it easier to navigate the undo history as a tree
structure.

    (add-hook 'org-mode-hook 'undo-tree-mode)

Prettify headings and plain lists in Org mode.

    (use-package "org-superstar")
    (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))


<a id="orgc4ddd44"></a>

## ttl-mode

i'm not super happy with this mode. might look for an alternative. The
indentation it aggressivly-forces is annoying.

    (add-hook 'ttl-mode-hook 'turn-on-font-lock)
    (add-to-list 'auto-mode-alist '("\\.\\(n3\\|ttl\\|trig\\)\\'" . ttl-mode))


<a id="org7cdf264"></a>

## projectile

install both projectile and its treemacs integration

    (use-package "projectile")
    (use-package "treemacs-projectile")

basic treemacs setup. from <https://github.com/bbatsov/projectile>

    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

to jump to any file in a project, use C-c p f


<a id="org0890d7a"></a>

## smart-parens-mode

    (use-package "smartparens")

The default slurping and barfing commands were being captured by my
terminal before smart parens mode could get them. This remapping
fixed that.

    (defun my-smartparens-mode-config ()
      "map slurping and barfing (because the default C-M-<right>/<left> were being capture by the terminal)"
      (local-set-key (kbd "M-<right>") 'sp-backward-barf-sexp)
      (local-set-key (kbd "M-<left>") 'sp-backward-slurp-sexp)
      (local-set-key (kbd "M-<backspace>") 'backward-kill-word))

    (add-hook 'smartparens-mode-hook 'my-smartparens-mode-config)

By default smartparens completes single-quotes with a matching
single-quote. This is annoying because I only use smart-parens for
lisps, and lisps use unpaired single-quotes to indicate data. So I want
to disable that autocompletion.

    (eval-after-load "smartparens" '(sp-pair "'" nil :actions :rem))

Also don't like it completing double-quotes

    (eval-after-load "smartparens" '(sp-pair "\"" nil :actions :rem))

Also disable the auto-completion of `` ` ``

    (eval-after-load "smartparens" '(sp-pair "`" nil :actions :rem))

Use strict-mode.

    (add-hook 'smartparens-mode-hook 'smartparens-strict-mode)


<a id="org867f6d2"></a>

## web-mode

I prefer web-mode to whatever the default mode was for dealing with
html.

Configure pairing and auto-closing.

    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-auto-close-style 2)
    (setq web-mode-code-indent-offset 2)

Require web-mode. I've commented this out because use-package is
erroring out

    (use-package web-mode)

Set various file-types to invoke web-mode

    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

The default indenting was too much for me, so I set it to 2 spaces.

    (setq web-mode-attr-indent-offset 2)

set "jsx" as content type with .js and .jsx files

    (setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))


<a id="org536bbfc"></a>

## whitespace-mode

For a while I thought I wanted to customize whitespace-mode and
start using it for programming. But I quickly realized that all I
really wanted to do was to automatically eliminate trailing
whitespace.

This is about as far as I got into customizing whitespace-mode. I
don't remember what it does, but I'm sure it's great.

    (setq whitespace-style '(face trailing empty))


<a id="org5f3fa74"></a>

## dired-mode

enable all-the-icons in dired mode&#x2026; but only if we're in a graphical frame.

    (use-package "all-the-icons")
    (use-package "all-the-icons-dired")
    (add-hook
     'after-make-frame-functions
     (lambda ()
       (if (display-graphic-p)
           (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))))


<a id="orgf815a82"></a>

## config for Ruby

Require enh-ruby-mode.

    (use-package "enh-ruby-mode")

Use enh-ruby-mode instead of ruby-mode. Among other things, it has
  better detection of syntax errors.

    (add-to-list
     'auto-mode-alist
     '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
    (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

Adds a function to be run with enh-ruby-mode which:

-   Sets "C-o" to jump to a new line
-   creates "M-x insert-pry" command which will insert "require 'pry';
    binding.pry". (This will cause the ruby interpreter to start the pry
    repl in the context of this line).

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


### defunct

1.  ruby-electric-mode-setup

    this was all for geting ruby-electric mode working, but I don't do
    much ruby anymore and it no longer seems go be in melpa

    Adds a hook to start ruby electric mode. Ruby electric mode will
    auto-complete brackets, parens, and do-end blocks.

        (add-hook 'enh-ruby-mode-hook 'ruby-electric-mode)

    Overshadow the ruby-electric-curlies function defined in
    ruby-electric-mode. I added a slight modification to the function to
    put the cursor in between the curly braces, padded with a space on
    either side (like "{ X }").

        (defun ruby-electric-mode-config ()
         (defun ruby-electric-curlies (arg)
           (interactive "*P")
           (ruby-electric-insert
            arg
            (cond
             ((ruby-electric-code-at-point-p)
              (save-excursion
        	(insert "}")
        	(font-lock-fontify-region (line-beginning-position) (point)))
              (cond
               ((ruby-electric-string-at-point-p) ;; %w{}, %r{}, etc.
        	(if region-beginning
        	    (forward-char 1)))
               (ruby-electric-newline-before-closing-bracket
        	(cond (region-beginning
        	       (save-excursion
        		 (goto-char region-beginning)
        		 (newline))
        	       (newline)
        	       (forward-char 1)
        	       (indent-region region-beginning (line-end-position)))
        	      (t
        	       (insert " ")
        	       (save-excursion
        		 (newline)
        		 (ruby-indent-line t)))))
               (t
        	(if region-beginning
        	    (save-excursion
        	      (goto-char region-beginning)
        	      (insert " "))
        	  (insert " "))
        	(insert " ")
        	(backward-char)
        	(and region-beginning
        	     (forward-char 1)))))
             ((ruby-electric-string-at-point-p)
              (let ((start-position (1- (or region-beginning (point)))))
        	(cond
        	 ((char-equal ?\# (char-before start-position))
        	  (unless (save-excursion
        		    (goto-char (1- start-position))
        		    (ruby-electric-escaped-p))
        	    (insert "}")
        	    (or region-beginning
        		(backward-char 1))))
        	 ((or
        	   (ruby-electric-command-char-expandable-punct-p ?\#)
        	   (save-excursion
        	     (goto-char start-position)
        	     (ruby-electric-escaped-p)))
        	  (if region-beginning
        	      (goto-char region-beginning))
        	  (setq this-command 'self-insert-command))
        	 (t
        	  (save-excursion
        	    (goto-char start-position)
        	    (insert "#"))
        	  (insert "}")
        	  (or region-beginning
        	      (backward-char 1))))))
             (t
              (delete-char -1)
              (ruby-electric-replace-region-or-insert))))))

    Add a hook so that when ruby-electric-mode starts, the
    ruby-electric-curlies function will be overshadowed. Without doing
    this the packaged version of the function takes precedence.

        (add-hook 'ruby-electric-mode-hook 'ruby-electric-mode-config)

2.  rspec integration

    I haven't used rspec in a while, and if I was using it now this would
    have to be different because I'm on nixos. But even then I would
    probably just use something like `entr` to run my test suite.

    I this fix from <https://github.com/pezra/rspec-mode> is supposed to fix
    a bug where rspec runs in zshell and doesn't work. I'm not sure if
    it's actually helping me or not, as I haven't put much time into
    getting rspec running in emacs.

        (defadvice rspec-compile (around rspec-compile-around)
          "Use BASH shell for running the specs because of ZSH issues."
          (let ((shell-file-name "/bin/bash"))
            ad-do-it))
        (ad-activate 'rspec-compile)


<a id="org2b291dd"></a>

## config for Clojure

start eldoc-mode in cider-mode. Eldoc shows doc strings in the
mini-buffer.

    (add-hook 'cider-mode-hook 'eldoc-mode)

Hook for rainbow-delimiters mode. Rainbow delimiters colors parens
based on nesting level.

    (use-package "rainbow-delimiters")
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

Hook for show parens mode. Show parens mode will highlight the
matching paren to the paren under the cursor

    (add-hook 'clojure-mode-hook 'show-paren-mode)

Hook for smartparens mode. Smartparens mode auto-completes parens, and
adds commands that make working with paren-heavy languages easier.

    (add-hook 'clojure-mode-hook 'smartparens-mode)

Tell the nrepl (which cider-mode users) to log protocol messages

    (setq nrepl-log-messages t)

Don't automatically open the cider repl in a new window.

    (setq cider-repl-pop-to-buffer-on-connect nil)

use clojure-refactor package, and set it to use dot prefix notation in requirements

    (use-package "clj-refactor")
    (setq cljr-favor-prefix-notation nil)


### cider-sms-all-tests

Command to run all the tests in a cider session, and send the
results as an sms message.

1.  requirements

        (use-package "dash")
        (use-package "cider")

2.  command

    Runs all tests in all namespaces connected to the current cider
    session. Sends an SMS notification to the number specified by
    `gf/sms-notification`. Contains a summary of results, and files
    with linenumbers where failures occured in the test suite.

    The lambda gets called repeatedly by the sub-process, but doesn't
    seem to have any useful data until `results` is present in `response`.

    Was written for a long-running test suite, so no command was
    written for running a single tests, or a single namespace. To
    instead run a single namespace, change `"op" "test-all"` to `"op"
        "test"`. And change `"ns" nil` to `"ns" <namespace>`.

        (defun cider-sms-all-tests ()
          "Runs all namespaces in the current running nrepl session, and sends a text
          message with the results"
          (interactive)
          (cider-nrepl-send-request `("op"      "test-all"
        			      "ns"      nil
        			      "tests"   nil
        			      "load?"   "true"
        			      "session" ,(cider-current-session))
        			    (lambda (response)
        			      (nrepl-dbind-response response (summary results)
        				(if results
        				    (progn
        				      (let ((total (nrepl-dict-get summary "test"))
        					    (pass (nrepl-dict-get summary "pass"))
        					    (fail (nrepl-dict-get summary "fail"))
        					    (failure-details (gf/file-line-context results)))
        					(gf/sms-notification
        					 (gf/fmt-results-and-failures
        					  total
        					  pass
        					  fail
        					  failure-details)))))))))

3.  formatting

    Format the test results into a string for the SMS message.

        (defun gf/fmt-results-and-failures (total pass fail failure-details)
          "Join the test summary and failures"
          (string-join
           (cons (gf/fmt-results total pass fail)
        	 (list (gf/fmt-failures failure-details)))
           "\n"))

        (defun gf/fmt-results (total pass fail)
          "Format test summary"
          (format "Cider Test Results: Total: %s, Passing: %s, Failing: %s" total pass fail))

        (defun gf/fmt-failures (file-line-contexts)
          "Format a list of failures as <file>:<line-number>"
          (string-join
           (cons "Failed At:"
        	 (-map (lambda (fl-ln-cxt)
        		 (format "%s:%s" (car fl-ln-cxt) (cadr fl-ln-cxt)))
        	       file-line-contexts))
           "\n"))

4.  data accessors/constructors

    The `nrepl-dict.el` package provides a dict datatype that's
    returned by the cider nrepl client.

        (defun gf/file-line-context (results)
          "Walk down the results tree to get file, line, and context, of each failure"
          (-flatten-n 2 (nrepl-dict-map
        		 (lambda (ns vars)
        		   (nrepl-dict-map
        		    (lambda (_var tests)
        		      (let* ((problems (cider-test-non-passing tests))
        			     (count (length problems)))
        			(-map 'gf/problem->file-line-context problems)))
        		    vars))
        		 results)))

        (defun gf/problem->file-line-context (problem)
          "Build a list of `(file line context)'"
          (let ((file (nrepl-dict-get problem "file"))
        	(line (nrepl-dict-get problem "line"))
        	(context (nrepl-dict-get problem "context")))
            (list file line context)))


<a id="org32adfb6"></a>

## config for emacs lisp

Add hook for smartparens mode. (see clojure config for explanation)

    (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)

Add hook for show parens mode (see clojure config)

    (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

Add hook for eldoc-mode (see clojure config)

    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

Add hook for rainbow delimiters mode (see clojure config)

    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

When in emacs-lisp-mode, this will check that a byte compiled version
of the current .el file exists, and if it does, it will
byte-compile. This is useful for keeping .el files from falling out of
date behind their byte-compiled versions.

    (defun byte-compile-current-buffer ()
      "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
      (interactive)
      (when (and (eq major-mode 'emacs-lisp-mode)
    	     (file-exists-p (byte-compile-dest-file buffer-file-name)))
        (byte-compile-file buffer-file-name)))

    (add-hook 'prog-mode-hook
    	  (lambda ()
    	    (add-hook 'after-save-hook 'byte-compile-current-buffer nil 'make-it-local)))


<a id="org39677a0"></a>

## config for scheme

Start rainbow-delimiters mode with scheme

    (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

Start smartparens-mode with scheme.

    (add-hook 'scheme-mode-hook 'smartparens-mode)


<a id="org0e696fc"></a>

## config for coffeescript

set coffee-mode to use a tab width of 2 spaces

    (add-hook 'coffee-mode-hook (lambda () (setq coffee-tab-width 2)))


<a id="org1a3bca6"></a>

## purescript

use purescript mode and configure the indentation mode.

    (use-package purescript-mode)
    (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)

replace all instances of "forall" with "∀", but only in
purescript-mode buffers.

    (add-hook 'purescript-mode-hook
      (lambda()
        (add-hook 'before-save-hook
    	      (lambda ()
    		(replace-all-in-buffer "forall" "∀")
    		(replace-all-in-buffer "->" "→")
    		(replace-all-in-buffer "=>" "⇒")
    		(replace-all-in-buffer "<-" "←")
    		(replace-all-in-buffer "::" "∷"))
    	      nil t)))

    (defun replace-all-in-buffer (original new)
      "Replace all occurances of original with new."
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward original nil t)
    	(replace-match new))))


### purescript-language-server integration

hook the lsp mode into purescript

    (add-hook 'purescript-mode-hook #'lsp)

the default cmd for starting the language server is
`purescript-language-server --stdio`, but I'm using it by invoking
yarn, inside a nix shell. So we need to override these variables,
which were added in this PR:
<https://github.com/emacs-lsp/lsp-mode/pull/1596/files>

The original values, as of the time of that PR are:

    (defcustom lsp-purescript-server-executable
      "purescript-language-server"
      "Arguments to pass to the server."
      :type 'string
      :risky t
      :group 'lsp-purescript)

    (defcustom lsp-purescript-server-args
      '("--stdio")
      "Arguments to pass to the server."
      :type '(repeat string)
      :risky t
      :group 'lsp-purescript)

    (defun lsp-purescript--server-command ()
      "Generate LSP startup command for purescript-language-server."
      (cons lsp-purescript-server-executable
    	lsp-purescript-server-args))

The full command we want to run is

    nix-shell --run 'yarn purescript-language-server --stdio'

So we'll override the first two of the relevent variables

    (setq lsp-purescript-server-executable "nix-shell")
    (setq lsp-purescript-server-args '("--run" "'yarn purescript-language-server --stdio'"))

But that didn't work. It just immediately exits with no info. I
don't know why.

So I can run this one instead one instead, which implies we're in a
nix-shell.

    (setq lsp-purescript-server-executable "yarn")
    (setq lsp-purescript-server-args '("purescript-language-server" "--stdio"))

Note that this (obviously) requires the purescript-language-server
package to be installed through yarn, plus it requires
purescript. Both of the following should succeed (in the nix shell):

    yarn purescript-language-server
    purs ide # this is what the the above command wraps


### pscide integration attempt

this method fails in this way: <https://github.com/purescript-emacs/psc-ide-emacs/issues/189>

    (use-package purescript-mode)
    (use-package psc-ide)
    (add-hook 'purescript-mode-hook
      (lambda ()
        (psc-ide-mode)
        (company-mode)
        (flycheck-mode)
        (turn-on-purescript-indentation)))

Apparently it's the result of json-encoding-pretty-print being set
to true, but I'm still going to leave this off for now because the
LSP-mode is working fine.


<a id="orga28e1ed"></a>

## language server protocol

this is used by several different language modes

    (use-package lsp-mode)

by default this uses Super in the prefix key, which I'm aleady using
for xmonad

    (setq lsp-keymap-prefix "C-c l")


<a id="org1860d84"></a>

## term-alert

These commands provide wrappers around the term-alert.el package,
which allows for an alert to be sent after commands complete in term-mode.

    (use-package "term-alert")

Define two notification commands. They are both expecting to be run in
a terminal mode. `term-alert-function` should be a buffer local
variable, so these set it each time they're called.

    (defun sms-alert-on-cmd-completion ()
      (interactive)
      (setq term-alert-function 'gf/sms-notify-term-alert)
      (term-alert-next-command-toggle 1))

    (defun email-alert-on-cmd-completion ()
      (interactive)
      (setq term-alert-function 'gf/email-notify-term-alert)
      (term-alert-next-command-toggle 1))

Functions to be wrapped in the above commands.

    (defun gf/email-notify-term-alert ()
        (mail)
        (mail-to) (insert goose/email)      ; my email address
        (mail-subject) (insert "[EMACS] command completion")
        (mail-send)
        (kill-this-buffer))

    (defun gf/sms-notify-term-alert ()
      (gf/sms-notification "Term command completed."))


<a id="org33d4496"></a>

## javascript

set indentation to 2 spaces

    (setq js-indent-level 2)

start flycheck in javascript

    (add-hook 'js2-mode-hook 'flycheck-mode)

use smartparens mode

    (add-hook 'js2-mode-hook 'smartparens-strict-mode)

use js2-mode instead of javascript mode

    (add-to-list
       'auto-mode-alist
       `(,(rx ".js$") . js2-mode))


<a id="org3c2c3ac"></a>

## haskell

Since intero-mode has been end-of-lifed, i'm trying out dante.

    (use-package dante

    :after haskell-mode
    :commands 'dante-mode
    :init
    (add-hook 'haskell-mode-hook 'flycheck-mode)
    (add-hook 'haskell-mode-hook 'dante-mode)
    (add-hook 'haskell-mode-hook (lambda () (setq dante-tap-type-time 1))))


### previously on&#x2026;

I used to use inter-mode for haskell but it was end-of-lifed in favor of
the haskell lsp mode. I never liked that haskell lsp mode. Dante comes
the closest to having all the features of intero-mode.

(use-package "intero")


<a id="org3bfa86b"></a>

## Scala

generic scala mode, not super useful.

    (use-package "scala-mode")


<a id="org879ad67"></a>

## elm

todo:

1.  install elm mode
2.  (add-to-list 'company-backends 'company-elm)
3.  look into ensure that elm-oracle is installed


<a id="org951f57d"></a>

## java

tried out meghanada, but it was preventing saves and giving me other issues.


### meghanada

Experimenting with this mode.

The following are from <https://github.com/mopemope/meghanada-emacs/blob/master/README.md>

meghanada-mode interfaces with a meghanada server, similar to
intero-mode for haskell (I'm assuming). but loading the java repos
at work basically crashes emacs

    (use-package "meghanada")

dependencies of meghanada. Specified here: <https://github.com/mopemope/meghanada-emacs>

    (use-package "cl-lib")
    (use-package "yasnippet")
    (use-package "company")
    (use-package "flycheck")

the next code block does the following:

-   use meghanada in java-mode
-   enable flycheck
-   set indentation levels
-   set locations of java and maven
-   autoformat code on save

    (add-hook 'java-mode-hook
         (lambda ()
           (meghanada-mode t)
           (flycheck-mode +1)
           (setq c-basic-offset 4)
           (setq meghanada-java-path "java")
           (setq meghanada-maven-path "mvn")
           ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
           ))


### java lsp

set up java lsp mode

    (use-package "lsp-mode")
    (use-package "company-lsp")
    (use-package "lsp-ui")
    (use-package "lsp-java")

    (add-hook 'java-mode-hook 'lsp)
    (add-hook 'java-mode-hook 'flycheck-mode)
    (add-hook 'java-mode-hook 'company-mode)

i've basically given in to intellij for java at work.


<a id="orgf557e35"></a>

## python

use a whitespace mode with python and convert tabs to spaces on saving

    (add-hook 'python-mode-hook
    	  (lambda ()
    	    (setq leerzeichen-line-feed-glyph (make-glyph-code ?  'leerzeichen))
    	    (leerzeichen-mode 't)
    	    ;; (add-hook 'before-save-hook (lambda ( ) (tabify (point-min) (point-max) 't)) nil 'local)
    	    ))


<a id="org1da0c35"></a>

## json

pretty print json files

    (setq json-encoding-pretty-print t)

    (setq json-reformat:indent-width 2)


<a id="org049c67e"></a>

## git

Set the magit bindings recommended in the magit tutorial

    (use-package "magit")
    (global-set-key (kbd "C-x g") 'magit-status)
    (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

require package for manipulating github pull requests from within
magit. This started causing magit-status to fail to be able to open
the minibuffer, so I've disabled it for the time being.

    (use-package "forge")


<a id="org40b2df8"></a>

## RSS

use elfeed for rss, and elfeed-org to organize rss feeds in an org
file

    (use-package "elfeed")
    (use-package "elfeed-org")

Initialize elfeed-org. This hooks up elfeed-org to read the configuration when elfeed
is started with `M-x elfeed`

    (elfeed-org)

Specify a number of files containing elfeed configuration. If not set
then the location below is used. Note: The customize interface is also
supported.

    (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))


<a id="orgdcb1356"></a>

## eshell

eshell can mess up some commands that are expecting piped input. Add
them to this list if they don't work as expected with pipes.

    (eval-after-load "esh-proc" '(add-to-list 'eshell-needs-pipe "entr"))


<a id="orgf633e5f"></a>

## Twilio sms

I'd like to use this dotenv package, but I need to figure out quelpa
first&#x2026; maybe. Either way I'm not doing it right now.

    (use-package dotenv
      :ensure nil
      :quelpa
      (dotenv :repo "pkulev/dotenv.el"
    	  :fetcher github :upgrade t))

Ensure that the json.el package is present, which the request.el
library uses to parse json responses.

    (use-package "json")

Ensure that the request-deferred.el package is present, which wraps
request.el in a deferred function from deferred.el

    (use-package "request-deferred")

Helper function used to generate the string expected by the
authentication header in using http basic authenticaiton.

    (defun gf/build-auth-hash (username password)
      (concat "Basic " (base64-encode-string (concat username ":" password) t)))

Core function that sends a request to the Twilio API. `sid` and
`token` must be aquired from [Twilio](http://twilio.com), and
`from-phone` must be verified.

    (defun gf/twilio-send-text (sid token from-phone to-phone msg)
      (deferred:$
        (request-deferred (concat "https://api.twilio.com/2010-04-01/Accounts/" sid "/Messages.json")
    		      :parser 'json-read
    		      :data `((To . ,to-phone)
    			      (From . ,from-phone)
    			      (Body . ,msg))
    		      :headers `((authorization . ,(gf/build-auth-hash sid token))))
        (deferred:nextc it
          (lambda (raw-response)
    	(let* ((response (request-response-data raw-response))
    	       (status (request-response-symbol-status raw-response))
    	       (oopsie (request-response-error-thrown raw-response))
    	       (err-msg (cdr (assoc 'message response))))
    	  (if oopsie (message "Twilio connection error: %S, %S" oopsie err-msg)
    	    (message "Twilio SMS status: %S" status)))))
        (deferred:error it
          (lambda (err)
    	(message "Request error: %S" err)))))

Command wrapping the `gf/twilio-send-text` function. Queries user in
minibuffer for a phone number and message to send an SMS
message.

    (defun send-sms ()
      (interactive)
      (let ((to-phone (read-from-minibuffer "Recipient's phone number: "))
    	(msg (read-from-minibuffer "Text message: ")))
        (gf/twilio-send-text env/twilio-sid
    			 env/twilio-token
    			 env/from-phone
    			 to-phone
    			 msg)))

Generic command for sending an sms message to `env/my-phone`

    (defun gf/sms-notification (msg)
      (gf/twilio-send-text env/twilio-sid
    		       env/twilio-token
    		       env/from-phone
    		       env/my-phone
    		       msg))


<a id="org72088d9"></a>

## custom functions

I wrote this because I missed Vi's "o" button which would create an
empty line below the current one, and jump to it without breaking the
current line. I may be duplicating some existing emacs command here.

    (defun jump-to-newline ()
        "Move to the end of the current line, then create a newline.
    \(Like \"o\" in Vi.\) I'm probably replicating a pre-existing command."
        (interactive)
        (move-end-of-line nil)
        (newline)
        (indent-for-tab-command))

I got this function from:
<http://ergoemacs.org/emacs/emacs_byte_compile.html>


### org-mode spreadsheet helper functions

I wrote these functions to help with calculating my work hours and
wages in an org-mode spreadsheet. I needed these to help calculate
values in spreadsheet cells.

Adds up the minutes in hh:mm formatted time string.

    (defun to-minutes (time-string)
      "Accepts a string of format '(h)h:mm' and returns total minutes"
      (string-match "\\([0-9]+\\):\\([0-9]\\{2,\\}\\)" time-string)
      (let ((hours (string-to-number (match-string 1 time-string)))
    	(minutes (string-to-number (match-string 2 time-string))))
        (if (> minutes 59)
    	(error (concat (number-to-string minutes) " is not between 0 and 59"))
          (+ minutes (* hours 60)))))

Takes a float representing minutes, and returns an hh:mm formatted
string.

    (defun number-to-time (number)
      "Converts a float into formatted string (hh:mm)"
      (let ((hours (/ number 60))
    	(minutes (% number 60)))
        (concat (format "%d" hours)
    	    ":"
    	    (format "%02d" minutes))))

Takes a list of times in the hh:mm format, and returns a sum in the same
format

    (defun sum-times (time-list)
      "Takes a list of times (hh:mm), and returns sum in the same format (hh:mm)"
      (number-to-time (apply '+ (mapcar 'to-minutes time-list))))

Takes a hh:mm formatted time string, converts it to total minutes, and
  then multiplies it by an hourly rate. Returns a string formatted
  like dollars but without the "$" (because org-mode cannot read from
  a spreadsheet cell starting with "$")

    (defun time-to-wage (time dollars-per-hour)
      "Converts time (hh:mm) to wages."
      (let ((minutes (to-minutes time)))
        (let ((hours (/ minutes
    		    60.0)))
          (format "%0.2f" (* hours dollars-per-hour)))))

Converts a float into dollar format ($0.00)

    (defun number-to-dollars (float)
      "Formats float into dollar string"
      (format "$%0.2f" float))


<a id="org07e5f2e"></a>

## novelty functions

These were the first functions I wrote, while reading the built-in
emacs lisp tutorial. A friend of mine loves the table-flipping meme,
but hates emacs. So I decided to write the table-flipping meme into
emacs.

(╯°□°)╯︵ ┻━┻
I started with this basic table-flipping character. Passing an
argument will specify how long to pause before flipping.

    (defun flip-table (num)
      "Animates flipping a table."
      (interactive "p")
      (let ((start-point (point))
    	(anticipation (or num 4)))
        (insert "(°-°) ┬─┬ ")
        (sit-for anticipation)
        (delete-region start-point (point))
        (insert "(╯°□°)╯︵ ┻━┻ ")))

flip-pɹoʍ︵$°□°$
My next function flips the last word before the cursor. A couple
required functions are also included.

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
