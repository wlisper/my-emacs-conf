(setq inhibit-startup-message t)
(display-time-mode 1)
(setq-default mode-line-format nil) ;; mode line
(fset 'yes-or-no-p 'y-or-n-p)
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; maximize window
;(set-face-attribute 'default nil :family "Courier 10 Pitch" :height 140)
(setq-default indent-tabs-mode nil)  ;; no tabs

(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

(defun show-toolbar()
  (interactive)
  (tool-bar-mode 1))
(defun hide-toolbar()
  (interactive)
  (tool-bar-mode -1))
(hide-toolbar)

(defun show-menu()
  (interactive)
  (menu-bar-mode 1))
(defun hide-menu()
  (interactive)
  (menu-bar-mode -1))
(hide-menu)

(defun show-scroll-bar()
  (interactive)
  (toggle-scroll-bar 1))
(defun hide-scroll-bar()
  (interactive)
  (toggle-scroll-bar -1))
(hide-scroll-bar)

(defun show-mode-line()
  (interactive)
  (setq-default mode-line-format t))
(defun hide-mode-line()
  (interactive)
  (setq-default mode-line-format nil))
(hide-mode-line)

(defun show-linum()
  (interactive)
  (global-linum-mode t))
(defun hide-linum()
  (interactive)
  (global-linum-mode -1))
(setq x-select-enable-clipboard t) ; global copy and paste

(defun nshell()
  (interactive)
  (let ((currentbuf (get-buffer-window (current-buffer)))
	(newbuf     (generate-new-buffer-name "*shell*")))
    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (eshell newbuf)))

(defun search-web()
  (interactive)
  (if (and mark-active (/= (point) (mark)))
      (let ((word (buffer-substring-no-properties (region-beginning) (region-end))))
          (eww (format "http://cn.bing.com/search?q=%s" word)))
      (let ((word (thing-at-point 'word 'no-properties)))
          (if (and (not (null word)) (stringp word))
              (eww (format "http://cn.bing.com/search?q=%s" word))
              (eww "www.bing.com")))))
(defun eclear ()
  (interactive)
  (setq inhibit-read-only t)
  (erase-buffer))
(global-set-key (kbd "C-c s") 'search-web)
(global-set-key (kbd "<f5>") 'revert-buffer)

(defun kill-other-buffers()
  (interactive)
  (mapc 'kill-buffer 
    (delq (current-buffer)
        (remove-if-not 'buffer-file-name (buffer-list)))))

(defun kill-all-buffers()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(use-package ztree
    :ensure t)

(use-package command-log-mode
    :ensure t)
(add-hook 'prog-mode-hook 'command-log-mode)

(use-package chinese-fonts-setup
    :ensure t)
(chinese-fonts-setup-enable)

(use-package color-theme
  :ensure t)
  (use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

(defun light-on()
  (interactive)
  (load-theme 'leuven t))

(defun light-out()
  (interactive)
  (load-theme 'tsdh-dark t))

(light-out)

(pdf-tools-install)
(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

(use-package try
	:ensure t)

(use-package which-key
      :ensure t 
      :config
      (which-key-mode))

(use-package org-password-manager
    :ensure t
    :config
    (add-hook 'org-mode-hook 'org-password-manager-key-bindings))

(setenv "BROWSER" "chromium-browser")

  	(use-package org-bullets
  	:ensure t
  	:config
  	(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

        (custom-set-variables
         '(org-directory "~/Org-mode/orgfiles")
         '(org-default-notes-file (concat org-directory "/notes.org"))
         '(org-export-html-postamble nil)
         '(org-hide-leading-stars t)
         '(org-startup-folded (quote overview))
         '(org-startup-indented t)
         )

        (setq org-file-apps
    	  (append '(
    		    ("\\.pdf\\'" . "evince %s")
    		    ) org-file-apps ))

        (global-set-key "\C-c a" 'org-agenda)

        (use-package org-ac
    	    :ensure t
    	    :init (progn
    		    (require 'org-ac)
    		    (org-ac/config-default)
    		    ))

        (global-set-key (kbd "C-c c") 'org-capture)

        (setq org-agenda-files (list "~/Org-mode/orgfiles/gcal.org"
    				 "~/Org-mode/orgfiles/i.org"
    				 "~/Org-mode/orgfiles/schedule.org"))
        (setq org-capture-templates
    			    '(("a" "Appointment" entry (file  "~/Org-mode/orgfiles/gcal.org" "Appointments")
    				     "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n")
    				    ("n" "Note" entry (file+headline "~/Org-mode/orgfiles/notes.org" "Notes")
    				     "* Note %?\n%T")
    				    ("l" "Link" entry (file+headline "~/Org-mode/orgfiles/links.org" "Links")
    				     "* %? %^L %^g \n%T" :prepend t)
    				    ("b" "Blog idea" entry (file+headline "~/org-mode/orgfiles/i.org" "Blog Topics:")
    				     "* %?\n%T" :prepend t)
    				    ("t" "To Do Item" entry (file+headline "~/org-mode/orgfiles/i.org" "To Do Items")
    				     "* %?\n%T" :prepend t)

    				    ("j" "Journal" entry (file+datetree "~/Org-mode/journal.org")
    				     "* %?\nEntered on %U\n  %i\n  %a")
                                       ("s" "Screencast" entry (file "~/Org-mode/orgfiles/screencastnotes.org")
                                       "* %?\n%i\n")))


    (defadvice org-capture-finalize 
        (after delete-capture-frame activate)  
      "Advise capture-finalize to close the frame"  
      (if (equal "capture" (frame-parameter nil 'name))  
  	(delete-frame)))

    (defadvice org-capture-destroy 
        (after delete-capture-frame activate)  
      "Advise capture-destroy to close the frame"  
      (if (equal "capture" (frame-parameter nil 'name))  
  	(delete-frame)))  

    (use-package noflet
      :ensure t )
    (defun make-capture-frame ()
      "Create a new frame and run org-capture."
      (interactive)
      (make-frame '((name . "capture")))
      (select-frame-by-name "capture")
      (delete-other-windows)
      (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
        (org-capture)))

(use-package ace-window
:ensure t
:init
(progn
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))) 
  ))

(use-package counsel
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
  :map ivy-minibuffer-map
  ("M-y" . ivy-next-line)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))


  (use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

(use-package yasnippet
  :ensure t
  :init
    (yas-global-mode 1))

(use-package auto-complete
    :ensure t
    :init
    (progn
        (ac-config-default)
        (global-auto-complete-mode t)))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package bison-mode
    :ensure t)

(use-package autopair
    :ensure t
    :config (autopair-global-mode 1))

(use-package projectile
    :ensure t)

(use-package psvn
    :ensure t)
(setq svn-status-verbose nil)

(use-package magit
    :ensure t
    :config (global-set-key (kbd "C-c m") 'magit-status))

(use-package cmake-mode
    :ensure)

(defun my:ac-c-header-init()
    (require 'ac-c-headers)
    (add-to-list 'ac-sources 'ac-source-c-headers)
    (add-to-list 'ac-sources 'ac-source-c-header-symbols t))
(use-package ac-c-headers
    :ensure t
    :config 
    (add-hook 'c++-mode-hook 'my:ac-c-header-init)
    (add-hook 'c-mode-hook   'my:ac-c-header-init))

;; flymake-google-cpplint hook function
(defun my:flymake-google-init()
    (require 'flymake-google-cpplint)
    (custom-set-variables
        '(flymake-google-cpplint-command "/opt/cpplint-1.3.0/cpplint.py"))
    (flymake-google-cpplint-load))   

;; install flymake-google-cpplint package
(use-package flymake-google-cpplint
    :ensure t
    :config
    (add-hook 'c-mode-hook 'my:flymake-google-init)
    (add-hook 'c++-mode-hook 'my:flymake-google-init))

;; install flymake-cursor package
(use-package flymake-cursor
    :ensure t)

;; install google-c-style package
(use-package google-c-style
    :ensure t
    :config
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(defun cedet-hook()
    (semantic-mode 1)
    (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'c-mode-common-hook 'cedet-hook)

(use-package semantic
    :ensure t)
(use-package semantic/bovine/gcc)
(use-package semantic/ia)
(defun c-semantic-hook()
    (setq semanticdb-default-save-directory (concat  "~/.emacs.d/semanticdb"))
    (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
    (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
    (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
    (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
    (semantic-mode t)
    (semantic-gcc-setup)
    (add-to-list 'ac-sources 'ac-source-functions)
    (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'c-semantic-hook)

(use-package irony
    :ensure t)

(add-hook 'c++-mode-hook 'iron-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(use-package php-mode
    :ensure t)
(use-package ac-php
    :ensure t)
(defun bs-php-mode-hook()
    (auto-complete-mode t)
    (require 'ac-php)
    (setq ac-sources '(ac-source-php))
    (yas-global-mode 1)
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 4)
    (setq php-template-compatibilite nil)
    (subword-mode 1))
(add-hook 'php-mode-hook 'bs-php-mode-hook)

(defun bs-web-mode-hook()
    (local-set-key '[backtab] 'indent-relative)
    (setq indent-tabs-mode nil)
    (setq web-mode-markup-indent-offset 4
          web-mode-css-indent-offset 4
          web-mode-code-indent-offset 4))
(add-hook 'web-mode-hook 'bs-web-mode-hook)

;(add-hook 'php-mode-hook 'my-php-mode-hook)
;(defun my-php-mode-hook ()
;  "My PHP mode configuration."
; (setq indent-tabs-mode nil
;        tab-width 4
;        c-basic-offset 4))

(use-package slime
    :ensure t
    :config 
    (progn
        (setq inferior-lisp-program "/usr/local/bin/sbcl")
        (setq slime-contribs '(slime-fancy))
        (require 'slime-autoloads)
        (require 'paredit)
        (add-hook 'slime-load-hook
            #'(lambda () (define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup)))))

(use-package ac-slime
    :ensure t)

(defun lisp-hook ()
  (paredit-mode t)
  (define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup)
  (make-variable-buffer-local 'show-paren-mode)
  (show-paren-mode 1))

(add-hook 'emacs-lisp-mode-hook 'lisp-hook)
(add-hook 'lisp-interaction-mode-hook 'lisp-hook)
(add-hook 'lisp-mode-hook 'lisp-hook)

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'slime-repl-mode))
(global-set-key (kbd "C-c C-h") 'slime-documentation)

(use-package paredit
    :ensure t)

(use-package clojure-mode
    :ensure t)
(use-package clojure-mode-extra-font-locking
    :ensure t)
(use-package cider
    :ensure t)

(use-package ido-completing-read+
    :ensure t)

(use-package smex
    :ensure t)

(use-package rainbow-delimiters
    :ensure t)

(use-package tagedit
    :ensure t)

(use-package nasm-mode
    :ensure t
    :config 
    (progn (add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))))

(use-package avy
:ensure t
:bind ("M-s" . avy-goto-word-1)) ;; changed from char as per jcs

(use-package neotree
    :ensure t
    :bind (("C-c d" . neotree-toggle))
    :config
    (setq neo-smart-open t))

(use-package bing-dict
    :ensure t
    :bind 
    (("C-x t" . bing-dict-brief))
)

(use-package cal-china-x
    :ensure t
    :config
    (progn 
        (setq mark-holidays-in-calendar t)
        (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
        (setq calendar-holidays cal-china-x-important-holidays)))

(use-package ox-reveal
:ensure ox-reveal)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

(use-package htmlize
:ensure t)

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

; Highlights the current cursor line
  (global-hl-line-mode t)
  
  ; flashes the cursor's line when you scroll
  (use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  ; (setq beacon-color "#666600")
  )
  
  ; deletes all the whitespace when you hit backspace or delete
  (use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))
  
  ; expand the marked region in semantic increments (negative prefix to reduce region)
  (use-package expand-region
  :ensure t
  :config 
  (global-set-key (kbd "C-=") 'er/expand-region))

(setq save-interprogram-paste-before-kill t)


(global-auto-revert-mode 1) ;; you might not want this
(setq auto-revert-verbose nil) ;; or this
(global-set-key (kbd "<f5>") 'revert-buffer)

; mark and edit all copies of the marked region simultaniously. 
(use-package iedit
:ensure t)

; if you're windened, narrow to the region, if you're narrowed, widen
; bound to C-x n
(defun narrow-or-widen-dwim (p)
"If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
(interactive "P")
(declare (interactive-only))
(cond ((and (buffer-narrowed-p) (not p)) (widen))
((region-active-p)
(narrow-to-region (region-beginning) (region-end)))
((derived-mode-p 'org-mode)
;; `org-edit-src-code' is not a real narrowing command.
;; Remove this first conditional if you don't want it.
(cond ((ignore-errors (org-edit-src-code))
(delete-other-windows))
((org-at-block-p)
(org-narrow-to-block))
(t (org-narrow-to-subtree))))
(t (narrow-to-defun))))

;; (define-key endless/toggle-map "n" #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing keymap, that's
;; how much I like this command. Only copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package web-mode
    :ensure t
    :config
	 (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	 (setq web-mode-engines-alist
	       '(("django"    . "\\.html\\'")))
	 (setq web-mode-ac-sources-alist
	       '(("css" . (ac-source-css-property))
		 ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-enable-auto-closing t))
(setq web-mode-enable-auto-quoting t) ; this fixes the quote problem I mentioned

(use-package less-css-mode
    :ensure t)
(use-package emmet-mode
    :ensure t)

(defun load-if-exists (f)
  "load the elisp file only if it exists and is readable"
  (if (file-readable-p f)
      (load-file f)))

(load-if-exists "~/Dropbox/shared/mu4econfig.el")
(load-if-exists "~/Dropbox/shared/tempstuff.el")
(load-if-exists "~/Dropbox/shared/not-for-github.el")
