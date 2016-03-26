;;; init.el --- Emacs configuration file. Time-stamp: <2016-03-26>

;; Copyright (c) 2012-2016 Jonathan Gregory

;; Code in this document is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This code is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:

;; This is a verbatim copy of my GNU Emacs configuration file. If you
;; have comments, please send them to <jgrg at autistici dot org>.

;;; Epigraph:

;; The keyboard is the weapon of an Emacs guru. Not as clumsy or as
;; random as a mouse. An elegant weapon for a more civilized age. --?

;; package archive

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(add-hook 'package-menu-mode-hook 'hl-line-mode)

(eval-when-compile
  (require 'use-package))

;; ==================================================================
;; ˚˚ appearance
;; ==================================================================

;; disable tool bar, scroll bar and tool tip

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;; font and frame settings

(defvar my-default-font t "Initial font setting.")

(setq my-fonts '((1 "Courier New" . ((set-face-attribute 'default nil :font "Courier New" :height 180)
				     (set-frame-size (selected-frame) 1256 747 t)))
		 (2 "Inconsolata" . ((set-face-attribute 'default nil :font "Inconsolata" :height 190)
				     (set-frame-size (selected-frame) 1258 747 t)))))

(when window-system
  (if my-default-font
      (let ((font-two (nth 2 (assoc 2 my-fonts)))
	    (frame-size (nth 3 (assoc 2 my-fonts))))
	(eval-expression font-two)
	(eval-expression frame-size)
	(setq my-default-font t))
    (let ((font-one (nth 2 (assoc 1 my-fonts)))
	  (frame-size (nth 3 (assoc 1 my-fonts))))
      (eval-expression font-one)
      (eval-expression frame-size)
      (setq my-default-font nil))))

(defun my-first-font ()
  (let ((fontname   (nth 1 (assoc 1 my-fonts)))
	(font-one   (nth 2 (assoc 1 my-fonts)))
	(frame-size (nth 3 (assoc 1 my-fonts))))
    (eval-expression font-one)
    (eval-expression frame-size)
    (setq my-default-font nil)
    (message "%s (done)" fontname)))

(defun my-second-font ()
  (let ((fontname   (nth 1 (assoc 2 my-fonts)))
	(font-two   (nth 2 (assoc 2 my-fonts)))
	(frame-size (nth 3 (assoc 2 my-fonts))))
    (eval-expression font-two)
    (eval-expression frame-size)
    (setq my-default-font t)
    (message "%s (done)" fontname)))

(defun switch-font (&optional arg)
  "Toggle between two fonts.
With a prefix ARG, prompt for a new font."
  (interactive "P")
  (if my-default-font
      (if arg
	  (progn
	    (let* ((font-list (font-family-list))
		   (completion-ignore-case t)
		   (new-font (completing-read "New font: " font-list)))
	      (set-frame-font new-font)
	      (jag/maximize-frame)
	      (message "%s (done)" new-font)))
	(my-first-font))
    (my-second-font)))

;; disable current theme before new one is loaded

(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; cycle through this set of themes

(setq my-themes '(bliss brin))
(setq my-cur-theme nil)

(defun cycle-my-theme (&optional arg)
  "Cycle through a list of themes defined by `my-themes'.
With a prefix ARG, cycle randomly through a list of available themes."
  (interactive "P")
  (if arg
      (let* ((list (custom-available-themes))
	     (theme (nth (random (length list)) list)))
	;; try not to repeat the same theme
	(if (string= theme (car custom-enabled-themes))
	    (let* ((new-list (remove theme (custom-available-themes)))
		   (new-theme (nth (random (length new-list)) new-list)))
	      (load-theme new-theme t)
	      (message "%s" new-theme))
	  (load-theme theme t)
	  (message "%s" theme)))
    (when my-cur-theme
      (disable-theme my-cur-theme)
      (setq my-themes (append my-themes (list my-cur-theme))))
    (setq my-cur-theme (pop my-themes))
    (load-theme my-cur-theme t)))

(when window-system
  (cycle-my-theme))

(bind-key "C-t" 'cycle-my-theme)

;; select a different theme

(defun select-theme ()
  "Select from a list of themes displayed in the minibuffer."
  (interactive)
  (let* ((themes (custom-available-themes))
	 (num (length themes))
	 (sort (cl-sort themes 'string-lessp))
	 (ido-grid-mode-padding "       ")
	 (ido-grid-mode-max-rows (+ 1 (/ num 4)))
	 (ido-grid-mode-max-columns (+ 1 4))
	 (theme (intern
		 (ido-completing-read "Load custom theme: "
				      (mapcar 'symbol-name sort)))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(bind-key "C-c t" 'select-theme)

;; ==================================================================
;; ˚˚ directory and load paths
;; ==================================================================

(setq user-emacs-directory (file-truename "~/.emacs.d/"))
(setq default-directory "~/Documents/org/")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; keep custom settings separate

(use-package custom
  :config
  (setq custom-file
	(expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; keep personal information separate

(use-package private)

;; ==================================================================
;; ˚˚ default settings
;; ==================================================================

;; (setq debug-on-error t)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq require-final-newline t)
(setq sentence-end-double-space nil)
(setq ad-redefinition-action 'accept)
(fset 'yes-or-no-p #'y-or-n-p)
(setq overflow-newline-into-fringe nil)
(setq display-time-format " %R ")
(setq display-time-default-load-average nil)
(setq display-time-24hr-format t)

;; backup settings

(setq backup-directory-alist '(("." . "~/Documents/org/.backups")))
(setq delete-old-versions t)
(setq delete-by-moving-to-trash t)
(setq version-control t)
(setq vc-make-backup-files t)

;;; recenter sequence
;; C-l cycles; C-M-l moves to the top

(setq recenter-positions '(top middle bottom))

;; move back and forth between places in the buffer
;; C-SPC C-SPC to mark, C-u C-SPC to jump back

(setq mark-ring-max 4)
(setq set-mark-command-repeat-pop t)

;; when popping the mark, continue popping until the cursor moves

(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; update timestamp when file is saved

(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-format "%04y-%02m-%02d")

;; remember point position

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saved-places"))

;; scratch buffer mode and message

(use-package org)

(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message
      (concat ";; GNU Emacs " emacs-version " (Org mode " org-version")\n\n"))

;; ==================================================================
;; ˚˚ files, projects, searches and commands
;; ==================================================================

;; helm for managing open files

(use-package helm
  :bind ("C-r" . helm-resume)
  :config
  (require 'helm-config)
  (helm-autoresize-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-ff-skip-boring-files t)
  (setq helm-org-headings-fontify t)
  (setq helm-org-show-filename nil)
  (setq helm-autoresize-max-height 50)
  (setq helm-autoresize-min-height 25)
  (bind-key "C-;" 'helm-org-in-buffer-headings org-mode-map)
  (bind-keys :map helm-map
	     ("C-e"   . helm-select-2nd-action)
	     ("C-M-n" . helm-select-8th-action))

  (defun helm-select-2nd-action (arg)
    "Select the 2nd action for the currently selected candidate."
    (interactive "P")
    (if arg
	(setq helm-bibtex-number-of-optional-arguments 2)
      (setq helm-bibtex-number-of-optional-arguments 1))
    (let ((n 2))
      (helm-select-nth-action (- n 1))))

  (defun helm-select-8th-action ()
    "Select the 8th action for the currently selected candidate."
    (interactive)
    (let ((n 8))
      (helm-select-nth-action (- n 1)))))

(use-package helm-swoop
  :bind ("C-c s" . helm-swoop)
  :config
  (setq helm-swoop-split-direction 'split-window-vertically)
  (setq helm-swoop-pre-input-function (lambda () ""))) ; disable pre-input

;; the silver searcher

(use-package helm-ag
  :bind ("C-c F" . helm-ag)
  :config (setq helm-ag-fuzzy-match t))

;;; isearch with an overview
;; also: C-l to recenter; M-q to query-replace

(use-package swiper
  :ensure t
  :bind ("C-s" . jag/swiper)
  :config
  (defun swiper-preview-file-outline ()
    "Show an outline of the current file in the minibuffer.
Count the number of matches and assign it to `ivy-height'. Then,
search the regexp using `swiper'."
    (let ((file (abbreviate-file-name (buffer-file-name))))
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(let ((val (count-matches "^;; ˚+ ")))
	  (setq ivy-height (+ val 1))))
      (swiper "^;; ˚+ ")))

  (defun jag/swiper (&optional arg)
    "Search selected region using `swiper'.
With a prefix ARG, search word at point. With two prefix arguments,
show an outline of the file in the minibuffer. Otherwise, prompt for a
string."
    (interactive "P")
    ;; reset ivy-height to its default value
    (let ((ivy-height 10))
      (if (eq (car current-prefix-arg) 16)
	  (swiper-preview-file-outline)
	(if arg
	    (swiper (thing-at-point 'word))
	  (if (use-region-p)
	      (let ((str (buffer-substring
			  (region-beginning)
			  (region-end))))
		(deactivate-mark)
		(swiper str))
	    (unless (> (window-size) (/ max-frame-height 2))
	      (setq ivy-height 5))
	    (swiper)))))))

;; search online

(use-package config-search)

;; projectile for managing large projects

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (setq projectile-mode-line
        '(:eval
          (format " Proj[%s]"
                  (projectile-project-name)))))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  (setq helm-projectile-sources-list
	'(helm-source-projectile-files-list helm-source-projectile-projects)))

;; remember recent and most frequent commands

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config (smex-initialize))

;;; switch between buffers, files and directories
;; M-n / M-p cycles through directories
;; C-d opens a dired buffer in the current directory
;; C-z prevents ido from switching directories during file name input

(use-package ido
  :config
  (ido-mode t)
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq org-completion-use-ido t)
  (setq org-outline-path-complete-in-steps nil)
  (setq-default read-buffer-completion-ignore-case t)

  ;; ido sort order priority
  (setq ido-file-extensions-order
        '(".org" ".tex" ".html" ".pdf" ".bib"))

  ;; ignore buffers, files and directories, press C-a to toggle
  (add-to-list 'ido-ignore-files "\\auto/")
  (add-to-list 'ido-ignore-directories "\\auto/")
  (add-to-list 'ido-ignore-files "\\.log" "\\.out")
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (add-to-list 'ido-ignore-files "\\.backups")
  (add-to-list 'ido-ignore-files "\\.Rhistory")
  (add-to-list 'ido-ignore-buffers "*Completions*"))

;; sort ido filelist by modification time instead of alphabetically

(use-package ido-sort-mtime
  :config (ido-sort-mtime-mode 1))

;; flex matching

(use-package flx-ido
  :config
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights
  (setq ido-use-faces nil))

;; display ido prospects in a grid

(use-package ido-grid-mode
  :ensure t
  :config
  (ido-grid-mode 1)
  (setq ido-grid-mode-keys nil)
  (setq ido-grid-mode-prefix-scrolls t)
  (setq ido-grid-mode-prefix ">> ")
  (set-face-foreground 'ido-first-match "lime green")
  ;; grid navigation
  (add-hook 'ido-setup-hook
            (lambda ()
              (bind-key "C-k" 'ido-grid-mode-down ido-completion-map)  ;;    i/p
              (bind-key "C-i" 'ido-grid-mode-up ido-completion-map)    ;;     |
              (bind-key "C-p" 'ido-grid-mode-up ido-completion-map)    ;; j <-+-> l
              (bind-key "C-n" 'ido-grid-mode-down ido-completion-map)  ;;     |
              (bind-key "C-j" 'ido-grid-mode-left ido-completion-map)  ;;    k/n
              (bind-key "C-l" 'ido-grid-mode-right ido-completion-map)
	      (bind-key [tab] 'ido-complete ido-completion-map)
	      (bind-key "C-M-j" 'ido-exit-minibuffer ido-completion-map)
              (bind-key "C-M-k" 'ido-kill-buffer-at-head ido-completion-map))))

;; move cursor to any position in the current view

(use-package avy
  :bind
  (("C-x SPC" . avy-goto-word-1)
   ("M-g g"   . avy-goto-line))
  :config
  (setq avy-keys-alist
	`((avy-goto-word-1 . (?j ?k ?l ?\; ?a ?s))
	  (avy-goto-line   . (?j ?k ?l ?\;))))
  (setq aw-keys '(?j ?o ?k ?\;)))

;; ==================================================================
;; ˚˚ buffer settings
;; ==================================================================

;; manage open buffers

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("Org" (or
			 (mode . org-mode)
			 (name . "^\\*Org Agenda")))
                 ("LaTeX" (or
			   (mode . latex-mode)
			   (mode . bibtex-mode)))
		 ("Mail" (or
			  (mode . mu4e-headers-mode)
			  (mode . mu4e-view-mode)
			  (mode . mu4e-compose-mode)))
		 ("Lisp" (mode . emacs-lisp-mode))
		 ("Dired" (mode . dired-mode))
		 ("Magit" (name . "\*magit"))))))

  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-switch-to-saved-filter-groups "default")
               (ibuffer-auto-mode 1)
               (hl-line-mode))))

;; make buffer names unique

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator " | "))

;; rename files and buffers
;; https://stackoverflow.com/questions/384284/how-do-i-rename-an-open-file-in-emacs

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
	  (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; avoid killing these buffers by accident

(defvar bury-buffer-list '("*scratch*" "*Messages*"))

(defun kill-or-bury-this-buffer ()
  "Kill or bury the current buffer.
See `bury-buffer-list' for a list of buffers to bury."
  (interactive)
  (if (member (buffer-name (current-buffer)) bury-buffer-list)
      (bury-buffer)
    (kill-buffer (current-buffer))))

(defun switch-to-scratch-and-back ()
  "Toggle between the *scratch* buffer and the current buffer.
If the *scratch* buffer does not exist, create one."
  (interactive)
  (let ((scratch-buffer-name (get-buffer-create "*scratch*")))
    (if (equal (current-buffer) scratch-buffer-name)
	(switch-to-buffer (other-buffer))
      (switch-to-buffer scratch-buffer-name)
      (lisp-interaction-mode))))

(defun kill-buffer-and-its-frame ()
  "Kill the current buffer and delete its frame."
  (interactive)
  (kill-buffer)
  (quit-window)
  (delete-frame))

;; kill buffer and its windows

(substitute-key-definition 'kill-buffer
                           'kill-buffer-and-its-windows
                           global-map)

(defun kill-other-buffer-and-window ()
  "Kill the next buffer in line and close its window."
  (interactive)
  (other-window 1)
  (if (member (buffer-name (current-buffer)) bury-buffer-list)
      (progn (bury-buffer)
	     (delete-window))
    (kill-buffer-and-window)))

;; ==================================================================
;; ˚˚ dired for managing directories
;; ==================================================================

(use-package dired-x
  :config
  (setq dired-omit-mode t)
  (setq dired-omit-size-limit nil)
  ;; guess default target directory when copying or renaming files
  (setq dired-dwim-target t)
  (setq dired-omit-files (concat dired-omit-files ; C-x M-o to toggle
                                 "\\|^#"
                                 "\\|.DS_Store$"
                                 "\\|.backups$"
                                 "\\|.localized$"
                                 "\\|.Rhistory$"))
  (add-hook 'dired-mode-hook 'hl-line-mode)
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode))))

(use-package dired+
  :config
  (setq diredp-wrap-around-flag nil))

;; reload dired buffer after making changes

(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
  (eval `(defadvice ,it (after revert-buffer activate)
           (revert-buffer))))

;; narrow a dired buffer to the files matching a fuzzy string

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("j" . dired-narrow-fuzzy))
  :config
  (setq dired-narrow-exit-action 'revert-buffer)
  (bind-keys :map dired-narrow-map
	     ("C-n" . dired-narrow-next-file)
	     ("C-p" . dired-narrow-previous-file)))

;; preview images in dired using qlmanage

(bind-key "<SPC>" (lambda () (interactive)
		    (start-process "preview" nil "qlmanage" "-p"
				   (dired-get-file-for-visit))) dired-mode-map)

;; open file using default application

(defun xah-open-in-external-app (&optional file)
  "Open the current FILE or dired marked files in external app.

The app is chosen from your OS's preference."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((not file) (list (buffer-file-name)))
           (file (list file)))))

    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ") ) )

    (when doIt
      (cond
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )

;; ==================================================================
;; ˚˚ key bindings
;; ==================================================================

(bind-key "C-o" 'ido-find-file)
(bind-key "C-M-o" 'helm-find-files)
(bind-key "C-x C-k" 'kill-or-bury-this-buffer)
(bind-key "C-c R" 'rename-file-and-buffer)
(bind-key "M-c" 'kill-ring-save)
(bind-key "M-m" 'capitalize-word)
(bind-key "M-v" 'yank)
(bind-key "C-c x" 'calendar)
(bind-key "C-c f" 'reveal-in-finder)
(bind-key "C-c <C-return>" 'xah-open-in-external-app)
(bind-key "C-c m" (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; evaluate buffer and region

(bind-keys :map emacs-lisp-mode-map
	   ("C-c e" . eval-buffer)
	   ("C-c r" . eval-region))

;; adjust font size

(bind-keys
 ("C-M--" . text-scale-decrease)
 ("C-M-=" . text-scale-increase))

;; bookmarks

(bind-keys
 ("C-1" . bmkp-bookmark-set-confirm-overwrite)
 ("C-2" . bookmark-bmenu-list)
 ("C-3" . helm-bookmarks)
 ("C-4" . headlong-bookmark-jump))

;; frame and window

(bind-keys
 ("M-0" . delete-window)
 ("M-1" . delete-other-windows)
 ("M-2" . vsplit-last-buffer)
 ("M-3" . hsplit-last-buffer)
 ("M-o" . other-window)
 ("C-c C-x r" . rotate-windows)
 ("C-c 0" . kill-buffer-and-its-frame)
 ("C-c k" . kill-other-buffer-and-window)
 ("M-`" . other-frame)
 ("M-)" . delete-frame)
 ("C-M-1" . shrink-window)
 ("C-M-2" . enlarge-window))

(bind-keys
 ("C-6" . jag/toggle-fullscreen)
 ("C-7" . jag/maximize-frame)
 ("C-c n" . jag/make-frame)
 ("C-x <up>" . jag/shrink-frame)
 ("C-x <down>" . jag/maximize-frame))

;; move forward and backward

(bind-keys
 ("C-M-e" . jag/forward-paragraph)
 ("C-M-a" . jag/backward-paragraph))

;; avoid backspace and return keys; use C-m or C-j instead

(bind-key "C-k" 'delete-line-no-kill)
(bind-key "M-h" 'backward-delete-word) ; use C-DEL to backward-kill-word
(bind-key "M-d" 'delete-word)	       ; instead of kill-word
(bind-key "M-DEL" 'backward-delete-word)
(bind-key "M-j" 'delete-backward-char)	; was indent-new-comment-line
(bind-key "µ" 'indent-new-comment-line)	; alt-m
(bind-key "C-j" 'org-return org-mode-map) ; instead of org-return-indent
(bind-key "C-M-p" 'org-toggle-link-display org-mode-map)

;; help commands

(bind-key "C-h h" 'helm-apropos)

;; avoid arrow keys when promoting and demoting lists

(bind-keys :map org-mode-map
	   ("∆" . org-metaup)		; alt-j
	   ("˚" . org-metadown)		; alt-k
	   ("˙" . org-shiftmetaleft)	; alt-h
	   ("¬" . org-shiftmetaright)	; alt-l
	   ("≤" . org-shiftleft)	; alt-,
	   ("≥" . org-shiftright))	; alt-.

;;; transpose words, sentences and paragraphs
;; use M-t to transpose-words and C-x C-t to transpose-sentences; use
;; alt-j / alt-k to transpose paragraphs in org-mode

(bind-key "C-x C-t" 'transpose-sentences) ; was transpose-lines

;; time and date

(bind-key "C-c i" 'insert-date)

;; unbind

(unbind-key "M-o" ibuffer-mode-map)
(unbind-key "C-o" ibuffer-mode-map)

(unbind-key "M-h" org-mode-map)
(unbind-key "C-'" org-mode-map)		; org-cycle-agenda-files
(unbind-key "C-," org-mode-map)		; org-cycle-agenda-files
(unbind-key "M-p" org-mode-map)		; org-shiftup
(unbind-key "C-c [" org-mode-map)	; org-agenda-file-to-front
(unbind-key "C-c C-j" org-mode-map)	; was org-goto

(unbind-key "C-o" dired-mode-map)	; use C-m instead
(unbind-key "M-p" dired-mode-map)
(unbind-key "M-c" dired-mode-map)
(unbind-key "C-M-j" dired-mode-map)

;; avoid arrow keys when switching buffers

(bind-keys
 ("M-∆" . previous-buffer)		; M-alt-j
 ("M-˚" . next-buffer))			; M-alt-k

;; use command as meta key and option for dead keys

(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; key chord

(use-package key-chord
  :commands key-chord-define
  :config
  ;; (key-chord-mode 1)
  (key-chord-define-global "jk" "Cape Town")
  (key-chord-define-global "jl" "South Africa")
  (key-chord-define-global "jj" (lambda() (interactive) (find-file "~/Documents/org/todo.org")))
  ;; avoid using the shift key
  (key-chord-define-global "1q" "!")
  (key-chord-define-global "2w" "@")
  (key-chord-define-global "3e" "#")
  (key-chord-define-global "4r" "$")
  (key-chord-define-global "5t" "%")
  (key-chord-define-global "6t" "^")
  (key-chord-define-global "6y" "^")
  (key-chord-define-global "7y" "&")
  (key-chord-define-global "8u" "*")
  (key-chord-define-global "9i" "(")
  (key-chord-define-global "0o" ")")
  (key-chord-define-global "-p" "_")
  (key-chord-define-global "[=" "+")
  (key-chord-define-global "./" "?")
  (key-chord-define-global "p[" "{"))

;; add prefix map

(bind-keys :prefix-map jag-prefix-map
	   :prefix "M-i"
	   ("c"   . calculator)
	   ("a"   . helm-bbdb)
	   ("b"   . boxquote-region)
	   ("u"   . boxquote-unbox)
	   ("w"   . display-time-world-and-focus)
	   ("f"   . switch-font)
	   ("j"   . switch-to-scratch-and-back)
	   ("M-q" . unfill-paragraph))

;; quickly switch between dictionaries

(bind-key "e" (lambda ()
                (interactive)
                (ispell-change-dictionary "british")) jag-prefix-map)
(bind-key "p" (lambda ()
                (interactive)
                (ispell-change-dictionary "brasileiro")) jag-prefix-map)

;; guide the following key bindings

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-max-description-length 50))

;; make bindings that stick around

(use-package hydra
  :ensure t)

;; smooth and in place scrolling

(use-package smooth-scroll
  :ensure t
  :diminish smooth-scroll-mode
  :config
  (smooth-scroll-mode t))

;; relative scrolling

(defvar max-frame-height 36
  "The maximum frame height.")

(defun jag/scroll-up ()
  "Scroll relative to the size of the current window (or frame).
The maximum frame height is defined by the variable
`max-frame-height'."
  (interactive)
  (let* ((full-size max-frame-height)
	 (half-size (/ full-size 2))
    	 (quarter-size (/ full-size 4)))
    (cond ((>= (window-size) full-size)
	   (scroll-up 5))
	  ((and (< (window-size) full-size)
		(> (window-size) half-size))
	   (scroll-up 4))
	  ((and (<= (window-size) half-size)
		(> (window-size) quarter-size))
	   (scroll-up 2))
	  ((<= (window-size) quarter-size)
	   (scroll-up 1)))))

(defun jag/scroll-down ()
  "Scroll relative to the size of the current window (or frame).
The maximum frame height is defined by the variable
`max-frame-height'."
  (interactive)
  (let* ((full-size max-frame-height)
	 (half-size (/ full-size 2))
    	 (quarter-size (/ full-size 4)))
    (cond ((>= (window-size) full-size)
	   (scroll-down 5))
	  ((and (< (window-size) full-size)
		(> (window-size) half-size))
	   (scroll-down 4))
	  ((and (<= (window-size) half-size)
		(> (window-size) quarter-size))
	   (scroll-down 2))
	  ((<= (window-size) quarter-size)
	   (scroll-down 1)))))

(bind-keys
 ("C-M-k" . jag/scroll-up)
 ("C-M-j" . jag/scroll-down))

(bind-keys*
 ("C-M-q" . beginning-of-buffer)
 ("C-M-w" . end-of-buffer))

(bind-key
 "C-M-SPC"
 (defhydra hydra-scroll (:pre (progn (smooth-scroll-mode 0)
				     (setq scroll-step 1))
			      :post (progn (smooth-scroll-mode t)
					   (setq scroll-step 0)))
   "scroll"
   ("j"   (jag/scroll-up) "up")
   ("SPC" (jag/scroll-up) "up")
   ("k"   (jag/scroll-down) "down")
   ("l"   nil "quit")
   ("q"   nil "quit")))

(bind-keys
 ("C-v" . jag/scroll-other-window)
 ("C-M-v" . jag/scroll-other-window-down))

(defun jag/scroll-other-window ()
  (interactive)
  (smooth-scroll/scroll-other-window 1))

 (defun jag/scroll-other-window-down ()
  (interactive)
  (smooth-scroll/scroll-other-window-down 1))

;; ==================================================================
;; ˚˚ citation, bibliography and cross-reference
;; ==================================================================

;; edit and validate BibTeX entries

(use-package config-bibtex)

;; org-ref for managing citations

(use-package org-ref
  :load-path "~/Documents/git/org-ref"
  :load-path "~/Documents/git/helm-bibtex"
  :init
  (use-package doi-utils
    :config
    (setq doi-utils-timestamp-format-function nil)
    (setq doi-utils-make-notes nil))
  (setq org-ref-bibliography-notes "~/Documents/org/annotation.org"
        org-ref-default-bibliography '("~/Documents/org/refs.bib")
        org-ref-pdf-directory "~/Documents/papers/")
  (setq org-ref-cite-onclick-function 'org-ref-cite-click-helm)
  (setq org-ref-insert-cite-function 'org-ref-helm-insert-cite-link)
  (setq org-ref-show-citation-on-enter nil)
  (setq org-ref-note-title-format
        "\n** $%a (%y) %t\n   :PROPERTIES:\n   :Custom_ID: %k\n   :END:\n")
  ;; custom open notes function
  (setq org-ref-open-notes-function
        (lambda nil
          (org-show-entry)
          (org-narrow-to-subtree)
          (show-children)
          (outline-previous-visible-heading 1)
          (recenter-top-bottom 0)
          (show-children)
	  (helm-bibtex-notes-mode 1)))

  (defun retrieve-bibtex ()
    (doi-utils-add-entry-from-crossref-query
     helm-input
     (car org-ref-default-bibliography))))

;;; helm-bibtex for managing bibliographies
;; press M-a to select all entries or C-SPC to mark entries
;; individually

(use-package helm-bibtex
  :load-path "~/Documents/git/helm-bibtex"
  :bind* ("C-c C-j" . helm-bibtex)
  :config
  (autoload 'helm-bibtex "helm-bibtex" "" t)
  (setq helm-bibtex-bibliography '("~/Documents/org/refs.bib"
				   "~/Documents/org/misc.bib")
        helm-bibtex-library-path "~/Documents/papers"
        helm-bibtex-notes-path "~/Documents/org/annotation.org")
  (setq helm-bibtex-full-frame nil)
  (setq helm-bibtex-number-of-optional-arguments 1)
  (setq helm-bibtex-cite-default-as-initial-input t)
  (setq helm-bibtex-additional-search-fields '(keywords))
  (setq helm-bibtex-notes-template-one-file
        "\n** $${author} (${year}) ${title}\n   :PROPERTIES:\n   :Custom_ID: ${=key=}\n   :END:\n\n")
  (setq helm-bibtex-fallback-options
        (quote (("Google Scholar" . "https://scholar.google.co.uk/scholar?q=%s")
		("Retrieve bibtex" . retrieve-bibtex)
		("Search notes" . helm-bibtex-search-notes-fallback))))

  ;; press C-o to go to the next source
  (defun helm-bibtex-search-notes-fallback ()
    "Search notes file."
    (let ((input (format "%s" helm-pattern)))
      (when (f-file? helm-bibtex-notes-path)
	(find-file helm-bibtex-notes-path)
	(goto-char (point-min))
	(swiper input))))

  ;; open with deafult pdf viewer
  (setq helm-bibtex-pdf-open-function
  	(lambda (fpath)
  	  (call-process "open" nil 0 nil "-a" "Skim.app" fpath)))

  ;; default action
  (helm-delete-action-from-source "Open PDF file (if present)" helm-source-bibtex)
  (helm-add-action-to-source "Open PDF file (if present)" 'helm-bibtex-open-pdf helm-source-bibtex 0)

  ;; format citation style
  (setq helm-bibtex-format-citation-functions
        '((org-mode . jag/helm-bibtex-format-citation-org-ref)
          (latex-mode . helm-bibtex-format-citation-cite))))

(use-package helm-bibtex-ext
  :bind ("C-c h" . helm-bibtex-show-notes)
  :config
  (setq helm-bibtex-notes-headline-level 2
	helm-bibtex-search-with "swiper"))

;; prompt once and use org-ref syntax

(defun jag/helm-bibtex-format-citation-org-ref (keys)
  "Formatter for `org-ref' citation commands.
Prompt for the command and additional arguments if the commands can
take any."
  (let* ((initial (when helm-bibtex-cite-default-as-initial-input helm-bibtex-cite-default-command))
         (default (unless helm-bibtex-cite-default-as-initial-input helm-bibtex-cite-default-command))
         (default-info (if default (format " (default \"%s\")" default) ""))
         (cite-command (completing-read
                        (format "Cite command%s: " default-info)
                        helm-bibtex-cite-commands nil nil initial
                        'helm-bibtex-cite-command-history default nil)))
    (if (member cite-command '("nocite" "supercite"))  ; These don't want arguments.
        (format "%s:%s" cite-command (s-join "," keys))
      (if (= helm-bibtex-number-of-optional-arguments 0)
          (format "%s:%s" cite-command (s-join "," keys))
        (if (= helm-bibtex-number-of-optional-arguments 1)
            (let ((pos (if (= helm-bibtex-number-of-optional-arguments 1)
                           (read-from-minibuffer "Postnote[1]: ") "")))
              (if (and (= helm-bibtex-number-of-optional-arguments 1) (string= "" pos))
                  (format "%s:%s" cite-command (s-join "," keys))
                (format "[[%s:%s][%s]]" cite-command (s-join "," keys) pos)))
          (let ((pre (if (= helm-bibtex-number-of-optional-arguments 2)
                         (read-from-minibuffer "Prenote[1]: ") ""))
                (pos (if (= helm-bibtex-number-of-optional-arguments 2)
                         (read-from-minibuffer "Postnote[2]: ") "")))
            (if (and (= helm-bibtex-number-of-optional-arguments 2) (string= "" pre) (string= "" pos))
                (format "%s:%s" cite-command (s-join "," keys))
              (format "[[%s:%s][%s::%s]]" cite-command (s-join "," keys) pre pos))))))))

;; ==================================================================
;; ˚˚ AUCTeX for managing (La)TeX files
;; ==================================================================

(use-package tex-site
  :ensure auctex)

(use-package latex
  :ensure auctex
  :init
  (load "auctex.el" nil t t)
  (setq TeX-parse-self t)                   ; enable parse on load
  (setq TeX-auto-save t)                    ; enable parse on save
  (setq TeX-PDF-mode t)                     ; instead of DVI mode
  (setq TeX-auto-untabify t)                ; remove tabs
  (setq reftex-plug-into-AUCTeX t)          ; RefTeX integration
  (add-hook 'TeX-mode-hook 'turn-on-reftex) ; turn RefTeX on
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (setq TeX-source-correlate-method 'auto)

  ;;; use Skim as default pdf viewer
  ;; option -b highlights the current line; option -g opens Skim in the background
  (setq TeX-view-program-list
        '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer"))))

(use-package server
  :init (server-start))

;; latex preview pane

(use-package latex-preview-pane
  :config
  (bind-key "C-c u" 'latex-preview-pane-mode LaTeX-mode-map))

;; set PATH

(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
(setq exec-path (append exec-path '("/usr/texbin")))

(use-package doc-view
  :config
  (setq doc-view-continuous t)
  (setq doc-view-resolution 300)
  ;; automatically update when making changes
  (add-hook 'doc-view-mode-hook 'auto-revert-mode))

(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/usr/local/bin") exec-path))

;; org mode latex exporter

(use-package ox-latex
  :config
  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))
  (add-to-list 'org-latex-classes
	       '("article"
		 "\\documentclass{article}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; beamer for creating slides in latex
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass\[presentation\]\{beamer\}"
                 ("\\section\{%s\}" . "\\section*\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}"))))

;; ignore headings tagged with `ignoreheading' when exporting to latex

(defun org-latex-ignore-heading-filter-headline (headline backend info)
  "Strip headline from HEADLINE. Ignore BACKEND and INFO."
  (when (and (org-export-derived-backend-p backend 'latex)
             (string-match "\\`.*ignoreheading.*\n" headline))
    (replace-match "" nil nil headline)))
(add-to-list 'org-export-filter-headline-functions
             'org-latex-ignore-heading-filter-headline)

;; better latex highlight in org-mode

(font-lock-add-keywords 'org-mode
                        '(("\\(\\\\cite\\)" . font-lock-keyword-face)
                          ("\\[\\([0-9]+\\)\\]" . font-lock-variable-name-face)
                          ("\\[\\([0-9]+-[0-9]+\\)\\]" . font-lock-variable-name-face) ;ie [34-35]
                          ("\\s-*[[:upper:]]+[a-zA-Z-]+[0-9]+[a-z]*" . font-lock-constant-face)))
(font-lock-add-keywords 'org-mode
                        '(("\\(\\\\citep\\)" . font-lock-keyword-face)))
(font-lock-add-keywords 'org-mode
                        '(("\\(\\\\citet\\)" . font-lock-keyword-face)))

;; ==================================================================
;; ˚˚ useful packages and modes
;; ==================================================================

;; smartparens

(use-package smartparens
  :ensure t
  :defer 2
  :diminish smartparens-mode
  :config (progn (require 'smartparens-config)
		 (require 'init-smartparens)
		 (smartparens-global-mode t))
  (setq sp-ignore-modes-list
	(delete 'minibuffer-inactive-mode sp-ignore-modes-list)))

;; expand abbreviations into templates

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (concat user-emacs-directory "snippets"))
  (yas-reload-all)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (unbind-key "C-c &" yas-minor-mode-map))

;; increase selected region by semantic units

(use-package expand-region
  :ensure t
  :bind (("C-M-3" . er/expand-region)))

;; auto completion

(use-package auto-complete
  :diminish
  auto-complete-mode
  :init
  (ac-config-default)
  (global-auto-complete-mode t)
  :config
  (bind-keys :map ac-complete-mode-map
	     ("C-n" . ac-next)
	     ("C-p" . ac-previous)))

(use-package auto-complete-config)
(use-package auto-complete-auctex)

;; save buffer modifications automatically

(use-package real-auto-save
  :diminish real-auto-save-mode
  :init
  (setq real-auto-save-interval 5)
  :config
  (add-hook 'prog-mode-hook 'real-auto-save-mode))

;; bookmark

(use-package bookmark+
  :config
  (setq bookmark-completion-ignore-case nil)
  (setq bookmark-save-flag 1)
  (bookmark-maybe-load-default-file))

;; visual line mode

(use-package simple
  :diminish (visual-line-mode auto-fill-function)
  :config (global-visual-line-mode t))

;; emacs lisp

(setq emacs-lisp-docstring-fill-column 70)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; learn emacs the hard way

(use-package guru-mode
  :diminish guru-mode
  :config (guru-global-mode 1))

;; markdown-mode

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

;; bookkeeping with ledger

(use-package ledger-mode
  :config
  (setq ledger-reconcile-default-commodity "£")
  (setq ledger-use-iso-dates t)
  ;; avoid error when exporting to html
  (setq max-specpdl-size 5200))

;; statistical programming

(use-package ess-site
  :mode
  (("\\.R$" . R-mode)
   ("\\.r$" . R-mode))
  :config (setq ess-eval-visibly-p nil)
  (setq ess-ask-for-ess-directory nil)
  (show-paren-mode 1))

;; edit files in the YAML data serialization format

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (bind-key "C-m" 'newline-and-indent yaml-mode-map))))

;; multiple major mode support for web editing

(use-package mweb-default-major-mode
  :defer t
  :config
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags
        '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
          (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
          (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  (multi-web-global-mode 1))

;; ==================================================================
;; ˚˚ news and mail
;; ==================================================================

;; newsreader client

(use-package gnus
  :config (use-package config-gnus)
  :bind ("C-c g" . gnus))

;; mail client

(use-package mu4e
  :init (use-package config-mu4e)
  :bind ("M-M"   . mu4e))

;; enable encryption

(use-package epa-file
  :defer t
  :config (epa-file-enable))

;; ==================================================================
;; ˚˚ music and audio
;; ==================================================================

;; enable multimedia support

(use-package emms
  :config (use-package config-emms))

;; timer

(use-package tea-time
  :bind ("C-c C-x C-;" . tea-time)
  :config
  (setq tea-time-sound "~/Documents/archive/audio/bell.wav")
  (setq tea-time-sound-command "mplayer -volume 0.5 %s"))

;; meditation timer

(use-package org-meditation
  :load-path "~/Documents/git/org-meditation"
  :config (bind-key "1" 'org-meditation org-agenda-mode-map))

;; transcribe audio

(use-package transcribe
  :config
  (setq transcribe-interviewer "Jonathan")
  (setq transcribe-interviewee "Interviewee"))

;; LilyPond for writing music scores

(use-package lilypond-mode
  :mode ("\\.ly$" . LilyPond-mode)
  :init
  (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
  (add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
  (setenv "PATH" (concat "/Applications/LilyPond.app/Contents/Resources/bin:/usr/bin" (getenv "PATH") ))
  (push "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp" load-path)
  (eval-after-load "LilyPond-mode"
    '(progn
       (load-library "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp/ac-lilypond.el")
       (bind-key [C-tab] 'LilyPond-autocompletion LilyPond-mode-map)))
  ;; press C-c C-s to view pdf
  (setq LilyPond-pdf-command "open -a 'Skim'"))

;; music programming with overtone

(use-package sclang
  :defer t
  :load-path "~/.emacs.d/scel/el"
  :config
  (setenv "PATH" (concat (getenv "PATH") ":/Applications/SuperCollider:/Applications/SuperCollider/SuperCollider.app/Contents/Resources"))
  (setq exec-path (append exec-path '("/Applications/SuperCollider"  "/Applications/SuperCollider/SuperCollider.app/Contents/Resources" ))))

(use-package clojure-mode
  :mode ("\.clj$" . clojure-mode))

(use-package cider
  :commands (cider-mode)
  :config (add-hook 'cider-mode-hook #'eldoc-mode))

;; ==================================================================
;; ˚˚ org-mode for managing notes, tasks and documents
;; ==================================================================

(use-package org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(bind-keys
 ("C-c l" . org-store-link)
 ("C-c a" . org-agenda)
 ("C-c b" . org-iswitchb))

;; load files automatically

(use-package org-checklist)
(use-package org-habit)
(use-package org-id)
(use-package org-mouse)
(use-package ox-beamer)
;; (use-package ox-org)
;; (use-package 'ox-odt)
;; (use-package 'org-contacts)

(setq org-confirm-babel-evaluate nil)
(setq org-tags-column -50)
(setq org-reverse-note-order t)
(setq org-return-follows-link t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(setq org-log-into-drawer t)
(setq org-cycle-global-at-bob t)
(setq org-id-method (quote uuidgen))
(setq org-id-link-to-org-use-id
      'create-if-interactive-and-no-custom-id) ; C-c l/C-c C-l
(setq org-habit-graph-column 55)
(setq org-habit-preceding-days 14)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-blank-before-new-entry '((heading . nil)
				   (plain-list-item . nil)))

(setq org-columns-default-format "%50ITEM(Task) %6Effort(Effort Estimate){:} %6CLOCKSUM(Actual Time)")
(setq org-global-properties (quote (("Effort_ALL" . "0:25 0:02 0:05 0:10 0:15 0:20 0:40 1:00 2:00 4:00")
                                    ("STYLE_ALL" . "habit"))))

;; archive and refile entries

(setq org-archive-location "%s_archive::* Archived Tasks")
(setq org-archive-reversed-order t)
(setq org-refile-targets (quote ((nil :maxlevel . 1))))

;;; renumber footnotes when new ones are inserted
;; press C-c C-x f to insert new reference and C-u C-c C-x f for a
;; list of options. Note that calling org-edit-footnote-reference (C-c
;; ') allows editing its definition.

(setq org-footnote-auto-adjust t)

;; org export settings

(setq org-export-with-tags 'not-in-toc)
(setq org-export-with-smart-quotes t)
(setq org-export-with-todo-keywords nil)

;; org babel language support

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (gnuplot . t)
         (sh . t)
         (org . t)
         (ledger . t)
         (lilypond . t)
         (plantuml . t)
         (clojure . t)
         (latex . t))))

;; make babel results block lowercase

(setq org-babel-results-keyword "results")

;; ditaa and plantUML for drawing diagrams

(setq org-ditaa-jar-path "~/.emacs.d/ditaa0_9/ditaa0_9.jar")
(setq ditaa-cmd "java -jar ~/.emacs.d/ditaa0_9/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/.emacs.d/plantuml.jar")

;; speed commands

(setq org-use-speed-commands t)

(add-to-list 'org-speed-commands-user '("d" org-todo "DONE"))
(add-to-list 'org-speed-commands-user '("x" org-todo "NEXT"))
(add-to-list 'org-speed-commands-user '("k" org-todo ""))
(add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
(add-to-list 'org-speed-commands-user '("r" call-interactively 'org-refile))
(add-to-list 'org-speed-commands-user '("A" call-interactively 'org-archive-subtree-default))
(add-to-list 'org-speed-commands-user '("D" call-interactively 'org-cut-subtree))
(add-to-list 'org-speed-commands-user '("i" call-interactively 'org-clock-in))
(add-to-list 'org-speed-commands-user '("o" call-interactively 'org-clock-out))
(add-to-list 'org-speed-commands-user '("n" call-interactively 'org-narrow-to-subtree))
(add-to-list 'org-speed-commands-user '("w" call-interactively 'widen))

(bind-key "i" 'org-agenda-clock-in org-agenda-mode-map)
(bind-key "o" 'org-agenda-clock-out org-agenda-mode-map)

;; ==================================================================
;; ˚˚ org agenda settings
;; ==================================================================

(setq org-agenda-files (quote ("~/Documents/org/todo.org"
                               "~/Documents/org/notes.org"
                               "~/Documents/org/fieldwork.org"
                               "~/Documents/org/annotation.org"
                               "~/Documents/org/draft.org"
			       "~/Documents/org/anth1004.org"
                               ;; "~/Documents/org/contacts.org"
			       "~/Documents/org/analysis.org")))
(setq org-agenda-remove-tags t)
(setq org-agenda-tags-column -125)
(setq org-agenda-skip-function
      '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELED" "DEFERRED")))
(setq org-deadline-warning-days 7)
(setq org-agenda-skip-deadline-if-done t)
(setq org-log-done 'time)
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-use-time-grid nil)
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;; extend current day to 1 hour after midnight

(setq org-extend-today-until 1)

;; display property drawer content in the agenda

(setq org-agenda-property-list
      (quote ("ADDRESS" "LOCATION" "At" "PHONE")))

;; custom agenda views

(use-package config-agenda)

;; clock report parameters

(setq org-agenda-clockreport-parameter-plist ;FIXME: agenda-with-archives has no effect
      '(:maxlevel 3 :scope agenda-with-archives :block thisweek :compact t :fileskip0 t :properties
                  ("Effort" "Difference" "Pomodoro") :formula "$3=$6-$2;T::$4=($6/25)*60;t"))

(setq org-clock-clocktable-default-properties ;FIXME: :scope doesn't update
      '(:maxlevel 3 :scope file :scope file :compact t :fileskip0 t :properties
                  ("Effort" "Difference" "Pomodoro") :formula "$2=$5-$1;T::$3=($5/25)*60;t"))

;; C-c a a R table alignment issue http://is.gd/Z2qHZj

(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "\\"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "--")))
      (concat str "__ "))))

(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)

;; mark task as done in the agenda buffer

(defun jag/org-agenda-mark-as-done ()
  "Mark item at point as done only if it is not scheduled to repeat."
  (interactive)
  (when (eq major-mode 'org-agenda-mode)
    (let ((agenda-buf (current-buffer)))
      (save-excursion
	(org-agenda-switch-to)
	(org-narrow-to-subtree)
	(goto-char (point-min))
	(if (re-search-forward ":repeat:" nil 'noerror)
	    (message "This item repeats. Press \"E\" to check or \":\" to remove restriction.")
	  (progn
	    (switch-to-buffer agenda-buf)
	    (org-agenda-todo "DONE")))
	(widen)
	(switch-to-buffer agenda-buf)))))

(bind-key "d" 'jag/org-agenda-mark-as-done org-agenda-mode-map)

;; move forward and backward by one agenda block

(defun jag/org-agenda-forward-block ()
  "Move forward by one agenda block.
Reposition the block to the top of the window."
  (interactive)
  (org-agenda-forward-block)
  (recenter-top-bottom 0))

(defun jag/org-agenda-backward-block ()
  "Move backward by one agenda block.
Reposition the block to the top of the window."
  (interactive)
  (org-agenda-backward-block)
  (recenter-top-bottom 0))

(bind-keys :map org-agenda-mode-map
	   ("C-M-n" . jag/org-agenda-forward-block)
	   ("C-M-p" . jag/org-agenda-backward-block))

;; a project is a project when it contain subtasks

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

;; a stuck project is a project whose next actions have not been defined

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

;; ==================================================================
;; ˚˚ GTD settings
;; ==================================================================

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("STARTED"  . (:foreground "SpringGreen2" :weight bold))
        ("REVISE"  . (:foreground "#E0CF9F" :weight bold))
        ("WAITING"  . (:foreground "#E0CF9F" :weight bold))
        ("DEFERRED"  . (:foreground "#E0CF9F" :weight bold))))

;; org-capture templates

(use-package org-capture
  :bind  ("C-c c" . org-capture)
  :config
  (setq org-capture-templates
	'(("t" "Task" entry (file+headline "~/Documents/org/todo.org" "Tasks")
	   "** TODO %^{Description} %^g\n" :prepend t)

	  ("r" "Reference" entry (file+headline "~/Documents/org/notes.org" "In-basket")
	   "** %^{Description} %^g\n%?" :prepend t)

	  ("b" "Bookmark" plain (file+headline "~/Documents/org/todo.org" "Bookmarks")
	   "- %?" :prepend t)

	  ("n" "Note" entry (file+headline "~/Documents/org/todo.org" "Notes")
	   "** %?%i\n%U" :prepend t)

	  ("l" "Ledger")

	  ("le" "Expenses" plain (file+headline "~/Documents/org/ledger.org" "Expenses")
	   "
#+name: expenses
#+begin_src ledger
%(org-read-date) * %^{Payed to}
    expenses:%^{Spent on|donation:|entertainment:|food:|groceries:|home:|other:|personal:|rent:|transportation:|utilities:electricity|utilities:gas|utilities:internet:|utilities:phone}%?  £%^{Amount}
    assets:%^{Debited from|bank:checking|bank:savings|cash}
#+end_src\n
" :prepend t)

	  ("li" "Income" plain (file+headline "~/Documents/org/ledger.org" "Income")
	   "
#+name: income
#+begin_src ledger
%(org-read-date) * %^{Received from}
    assets:%^{Credited into|bank:checking|bank:savings|cash}
    income:%^{In reference to|salary:|gig}%?  £%^{Amount}
#+end_src\n
" :prepend t)

	  ("d" "Diary" entry (file+headline "~/Documents/org/fieldwork.org" "Diary")
	   "** %(format-time-string \"%Y-%m-%d %b %H:%M\")\n\n%? %^{Effort}p" :clock-in t)

	  ("f" "Fieldnote" entry (file+headline "~/Documents/org/fieldwork.org" "Fieldnotes")
	   "** %(format-time-string \"%d %b %Y\")\n%? %^{Effort}p" :clock-in t)

	  ("c" "Contact" entry (file+headline "~/Documents/org/contacts.org" "Contacts")
	   "** %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:PHONE: %^{phone}
:NOTES: %^{notes}
:END:" :prepend t)

	  ("a" "Appt" entry (file+headline "~/Documents/org/todo.org" "Appointments")
	   "** %^{Description}\n:PROPERTIES:\n:At: %^{At}\n:END:\n%^t\n%?" :prepend t)

	  ("h" "Habit" entry (file+headline "~/Documents/org/todo.org" "Habits")
	   "** TODO %?\n   SCHEDULED: %t\n:PROPERTIES:\n:STYLE: habit\n:END:" :prepend t)

	  ("&" "Email" entry (file+headline "~/Documents/org/todo.org" "Tasks")
	   "** TODO %? %(org-set-tags-to \"mail\")\n%a" :prepend t)

	  ("^" "Email appt" entry (file+headline "~/Documents/org/todo.org" "Appointments")
	   "** %?\n:PROPERTIES:\n:At: %^{At}\n:END:\n%^t\n%a" :prepend t)

	  ("#" "Hold" entry (file+headline "~/Documents/org/todo.org" "Tickler")
	   "** TODO delete %a %(org-set-tags-to \"mail\")\n   SCHEDULED: %^t\n" :prepend t :immediate-finish t)

	  ("F" "Fiona" entry (file+headline "~/Documents/org/orientation.org" "2016")
	   "** Fiona %u\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:\n%?" :prepend t)

	  ("I" "Ioannis" entry (file+headline "~/Documents/org/orientation.org" "2016")
	   "** Ioannis %u\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:\n%?" :prepend t)))

  ;; capture context
  (setq org-capture-templates-contexts
	'(("#" ((in-mode . "mu4e-view-mode")))
	  ("&" ((in-mode . "mu4e-view-mode")))
	  ("^" ((in-mode . "mu4e-view-mode")))
	  ("F" ((in-file . "orientation.org")))
	  ("I" ((in-file . "orientation.org"))))))

;; ==================================================================
;; ˚˚ org mode extension
;; ==================================================================

;; deft for browsing org files

(use-package deft
  :bind ("C-c d" . deft)
  :config
  (setq deft-extensions '("org"))
  (setq deft-directory "~/Documents/org")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (add-hook 'deft-mode-hook 'hl-line-mode))

;; pomodoro technique

(use-package org-pomodoro
  :bind ("∏" . org-pomodoro) ; S-alt-p
  :config
  (setq org-pomodoro-long-break-frequency 4
	org-pomodoro-long-break-length 20)
  (setq org-pomodoro-show-number t)
  (setq org-pomodoro-expiry-time 180)
  (setq org-pomodoro-audio-player "mplayer")
  (setq org-pomodoro-finished-sound-args "-volume 0.5"
	org-pomodoro-long-break-sound-args "-volume 0.5"
	org-pomodoro-short-break-sound-args "-volume 0.5")

  (add-hook 'org-pomodoro-started-hook
	  (defun org-pomodoro-kitchen-timer ()
	    (let ((sound-file "~/Documents/archive/audio/timer-ticking.mp3"))
	      (call-process-shell-command
	       (concat "mplayer " (expand-file-name sound-file)) nil 0))))

  (add-hook 'org-pomodoro-killed-hook
	    (defun org-pomodoro-kill-kitchen-timer ()
	      (shell-command "killall mplayer"))))

;; wrap text with punctation

(use-package wrap-region
  :diminish wrap-region-mode
  :config
  (wrap-region-mode t)
  (add-hook 'org-mode-hook 'wrap-region-mode)
  (wrap-region-add-wrapper "*" "*" nil 'org-mode)
  (wrap-region-add-wrapper "/" "/" nil 'org-mode)
  (wrap-region-add-wrapper "_" "_" nil 'org-mode)
  (wrap-region-add-wrapper "=" "=" nil 'org-mode)
  (wrap-region-add-wrapper "+" "+" nil 'org-mode)
  (wrap-region-add-wrapper "~" "~" nil 'org-mode))

;; org tree slide for making presentations

(use-package org-tree-slide
  :config
  (bind-key "<f8>" 'org-tree-slide-mode org-mode-map)
  (bind-keys :map org-tree-slide-mode-map
	     ("<right>" . org-tree-slide-move-next-tree)
	     ("<left>" . org-tree-slide-move-previous-tree))
  (setq org-tree-slide-slide-in-effect nil)
  (setq org-tree-slide-fold-subtrees-skipped nil))

;; yank clipboard url as org-mode link

(use-package org-cliplink
  :config
  (bind-key "C-x l" 'org-cliplink org-mode-map))

;; plot tables in org-mode

(use-package gnuplot-mode
  :defer t
  :config
  (setq gnuplot-flags "-persist -pointsize 2"))

;; publish org-mode files to html

(use-package ox-publish
  :bind
  (:map org-mode-map
	("M-P"   . jag/org-publish-current-file)
	("M-i P" . org-publish-current-project))
  :config
  (setq org-publish-project-alist
      '(("org"
         :base-directory "~/Documents/web"
         :base-extension "org"
         :publishing-directory "~/Documents/web"
         :publishing-function org-html-publish-to-html
         :recursive t
         :headline-levels 3
         :auto-preamble t)
        ("website" :components ("org")))))

(defun jag/org-publish-current-file (&optional force async)
  "Publish the current file.
With prefix argument FORCE, force publish the file.  When
optional argument ASYNC is non-nil, publishing will be done
asynchronously, in another process."
  (interactive "P")
  (save-excursion                       ;restore point position
    (let ((file (buffer-file-name (buffer-base-buffer))))
      (if async
	  (org-export-async-start (lambda (results) nil)
	    `(let ((org-publish-use-timestamps-flag
		    (if ',force nil ,org-publish-use-timestamps-flag)))
	       (org-publish-file ,file)))
	(save-window-excursion
	  (let ((org-publish-use-timestamps-flag
		 (if force nil org-publish-use-timestamps-flag)))
	    (org-publish-file file)))))))

;; embbed youtube videos with org-mode links

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url (concat "https://www.youtube.com/embed/" handle)))
 (lambda (path desc backend)
   (cl-case backend
     ;; You may want to change your width and height.
     (html (format "<iframe width=\"440\" height=\"335\" src=\"https://www.youtube.com/embed/%s\" frameborder=\"0\" allowfullscreen>%s</iframe>"
                   path (or desc "")))
     (latex (format "\href{%s}{%s}" path (or desc "video"))))))

;; toggle inline images

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-M-i") 'org-toggle-inline-images)))

(add-hook 'org-babel-after-execute-hook
          (lambda () (org-display-inline-images nil t)))

;; ==================================================================
;; ˚˚ writing, editing and version control
;; ==================================================================

;; keep track of revisions

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell))

;; compare file differences and merge changes

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

;; treat undo history as tree using C-x u

(use-package undo-tree
  :bind (("M-z" . undo-tree-undo)
         ("M-r" . undo-tree-redo))
  :config
  (global-undo-tree-mode)
  (setq undo-tree-mode-lighter ""))

;; browse kill-ring list

(use-package browse-kill-ring
  :config
  (setq browse-kill-ring-highlight-current-entry t
        browse-kill-ring-display-duplicates nil)
  :bind ("C-c y" . browse-kill-ring))

;; move line or region using M-up M-down

(use-package move-text
  :config
  (move-text-default-bindings))

;; typopunct for dealing with hyphens and smart quotes

(use-package typopunct
  :config (typopunct-change-language 'english t))

;; insert dummy text

(use-package lorem-ipsum
  :bind ("C-x i" . Lorem-ipsum-insert-paragraphs)
  :config (setq lorem-ipsum-sentence-separator " "))

;; highlight passive voice, duplicate words, and weasel words

(use-package writegood-mode
  :defer t)

;; distraction-free writing for Emacs

(use-package writeroom-mode
  :bind ("C-c w" . writeroom-mode))

;; delete word without killing it
;; http://www.emacswiki.org/emacs/BackwardDeleteWord

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

;;; delete line but don't kill it
;; use kill-whole-line (C-S-backspace) instead; for sentences, use
;; kill-sentence (M-k)

(defun delete-line-no-kill ()
  "Delete line but don't kill it."
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1))

;; adjust space when killing or deleting words
;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-editing.el

(defun modi/just-one-space-post-kill-word (&rest _)
  "Function to manage white space after `kill-word' or `delete-word' operations.
1. If point is at the beginning of the line after possibly some white space,
   remove that white space and re-indent that line.
2. If there is space before or after the point, ensure that there is only
   one white space around the point.
3. Otherwise, do nothing.
During the whole operation do not change the point position with respect to the
surrounding white space.
abc|   def  ghi <-- point on the left of white space after 'abc'
abc| ghi        <-- point still before white space after calling this function
abc   |def  ghi <-- point on the right of white space before 'def'
abc |ghi        <-- point still after white space after calling this function."
  (save-excursion ; maintain the initial position of the pt with respect to space
    (cond ((looking-back "^ *") ; remove extra space at beginning of line
           (just-one-space 0)
           (indent-according-to-mode))
          ((or (looking-at   " ")
               (looking-back " ")) ; adjust space only if it exists
           (just-one-space 1))
          (t ; do nothing otherwise, includes case where the point is at EOL
           ))))
;; Delete extra horizontal white space after `kill-word' and `backward-kill-word'
(advice-add 'kill-word :after #'modi/just-one-space-post-kill-word)
(advice-add 'delete-word :after #'modi/just-one-space-post-kill-word)
(advice-add 'backward-delete-word :after #'modi/just-one-space-post-kill-word)

;;; jump to the end of the paragraph
;; with sentence-end-double-space set to nil, pressing C-a / C-e moves
;; point to the beginning or end of the SENTENCE. Pressing C-M-a /
;; C-M-e will move it to the beginning or end of the PARAGRAPH.

(defadvice jag/forward-paragraph (around real-forward)
  "Consider a sentence to have one space at the end."
  (let ((sentence-end-double-space t))
    ad-do-it))

(defadvice jag/backward-paragraph (around backward-forward)
  "Consider a sentence to have one space at the end."
  (let ((sentence-end-double-space t))
    ad-do-it))

(defun jag/forward-paragraph ()
  (interactive)
  (org-forward-sentence))

(defun jag/backward-paragraph ()
  (interactive)
  (org-backward-sentence))

;; unfill paragraph undoes M-q

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;; upcase (M-u), lowercase (m-l) and capitalize (M-m) word or region
;; https://github.com/snosov1/dot-emacs

(defmacro action-dispatch (action)
  `(defun ,(intern (format "%s-dispatch" action)) (arg)
     "Perform action on word or region."
     (interactive "P")
     (if (region-active-p)
         (,(intern (format "%s-region" action)) (region-beginning) (region-end))
       (,(intern (format "%s-word" action)) (if arg arg 1)))))

(define-key global-map [remap upcase-word]     (action-dispatch upcase))
(define-key global-map [remap downcase-word]   (action-dispatch downcase))
(define-key global-map [remap capitalize-word] (action-dispatch capitalize))

;;; comment and copy with C-u M-;
;; https://github.com/snosov1/dot-emacs

(defun comment-region-as-kill (beg end)
  (copy-region-as-kill beg end)
  (comment-region beg end))

(define-key global-map (kbd "M-;")
  (defun comment-dwim-or-comment-region-as-kill (arg)
    (interactive "*P")
    (if (equal current-prefix-arg '(4))
        (comment-region-as-kill (region-beginning) (region-end))
      (comment-dwim arg))))

;; split line without moving the point

(defun jag/split-line ()
  "Split the current line without moving the point.
The text to the right of the point is pushed down one line.
Press \\[delete-char] to bring the text back up."
  (interactive)
  (save-excursion
    (newline)))

(bind-key "C-M-;" 'jag/split-line)

;; aspell for spell checking

(use-package ispell
  :bind
  (("M-4" . ispell-word)
   ("M-5" . ispell-region)
   ("M-6" . ispell-buffer))
  :init
  (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-list-command "list")
  (setq ispell-local-dictionary "british")
  (setq ispell-extra-args '("--sug-mode=ultra"))

  ;; prevent ispell from checking code blocks and citations
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+begin_example " . "#\\+end_example$"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE " . "#\\+END_EXAMPLE$"))
  (add-to-list 'ispell-skip-region-alist '("\\\\cite.*{" . "}"))
  (add-to-list 'ispell-skip-region-alist '("\\(cite.:[[:upper:]]+[0-9]+[a-z]\\)")))

;; ==================================================================
;; ˚˚ dictionary, thesaurus and translation tools
;; ==================================================================

(key-chord-define-global
 "ww"
 (defhydra hydra-words (:color teal :hint nil)
   "
^Dictionary^               ^Thesaurus^             ^Translation^             ^Other
------------------------------------------------------------------------------------------
  _d_: dictionary          _t_: thesaurus          _e_: translate to en      _h_: helm-dictionary
  _s_: search word         _o_: oxford thesaurus   _P_: translate to pt      _w_: wikipedia
  _a_: portuguese          _T_: search online      _E_: open browser         _q_: quit
_M-s_: search online       ^ ^                     _P_: open browser
"
   ("d" osx-dictionary-search-pointer)
   ("s" osx-dictionary-search-input)
   ("a" osx-dictionary-second-dictionary-search-input)
   ("M-s" dictionary-search)

   ("t" thesaurus)
   ("o" osx-dictionary-thesaurus-search-input)
   ("T" thesaurus-search)

   ("e" google-translate-at-point)
   ("p" google-translate-at-point-reverse)
   ("E" translate-to-en)
   ("P" translate-to-pt)

   ("h" helm-dictionary)
   ("w" wiki-summary)
   ("q" nil)))

(use-package osx-dictionary
  :config
  (defadvice osx-dictionary-second-dictionary-search-input (around osx-dictionary-dictionary-choice)
    (let ((osx-dictionary-dictionary-choice "Portuguese"))
      ad-do-it))

  (defun osx-dictionary-second-dictionary-search-input ()
    (interactive)
    (osx-dictionary-search-input))

  (defadvice osx-dictionary-thesaurus-search-input (around osx-dictionary-dictionary-choice)
    (let ((osx-dictionary-dictionary-choice "British English Thesaurus"))
      ad-do-it))

  (defun osx-dictionary-thesaurus-search-input ()
    (interactive)
    (osx-dictionary-search-pointer)))

;; additional dictionaries

(use-package helm-dictionary
  :defer t
  :init
  (setq helm-dictionary-database "~/.emacs.d/en-pt-dictionary")
  (setq helm-dictionary-online-dicts
	'(("linguee" . "http://www.linguee.com/english-portuguese/search?source=auto&query=%s"))))

;; thesaurus

(use-package synonyms
  :config
  (setq synonyms-file "~/Documents/archive/mthesaur.txt")
  (setq synonyms-cache-file "~/.emacs.d/mthesaur.cache")

  (defun thesaurus ()
    (interactive)
    (let ((word (thing-at-point 'word)))
      (synonyms nil word)
      (windmove-down))))

(defun thesaurus-search ()
  "Look up word at point in an online thesaurus."
  (interactive)
  (browse-url
   (format
    "http://www.thesaurus.com/browse/%s"
    (thing-at-point 'word))))

;; translation

(use-package google-translate
  :config
  (setq google-translate-default-source-language "pt")
  (setq google-translate-default-target-language "en")
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-translation-directions-alist
        '(("pt" . "en") ("en" . "pt"))))

(defun translate-to-pt ()
  "Translate from English to Portuguese by querying word at point.
If region is active, use that instead."
  (interactive)
  (browse-url
   (format
    "https://translate.google.com/#en/pt/%s"
    (if (use-region-p)
        (url-hexify-string (buffer-substring
                            (region-beginning)
                            (region-end)))
      (thing-at-point 'word)))))

(defun translate-to-en ()
  "Translate from Portuguese to English by querying word at point.
If region is active, use that instead."
  (interactive)
  (browse-url
   (format
    "https://translate.google.com/#pt/en/%s"
    (if (use-region-p)
        (url-hexify-string (buffer-substring
                            (region-beginning)
                            (region-end)))
      (thing-at-point 'word)))))

;; ==================================================================
;; ˚˚ useful functions
;; ==================================================================

;;; download and watch youtube videos from emacs
;; http://oremacs.com/2015/01/05/youtube-dl/
;; https://github.com/rg3/youtube-dl

(defun youtube-to-mp4 ()
  "Download youtube video to disk as an mp4 file using youtube-dl.
Append a date to it using date2name."
  (interactive)
  (let* ((str (current-kill 0))
         (default-directory "~/downloads/youtube")
         (proc (get-buffer-process (ansi-term "/bin/bash"))))
    (term-send-string
     proc
     (concat "cd ~/downloads/youtube/tmp && youtube-dl -o '%(title)s.%(ext)s' --add-metadata " str "--restrict-filenames" "\n"
             "date2name -c *.mp4" "\n"
             "mv *mp4 ~/downloads/youtube" "\n"))))

(defun youtube-to-mp3 ()
  "Download youtube video to disk as an mp3 file using youtube-dl.
Append a date to it using date2name."
  (interactive)
  (let* ((str (current-kill 0))
         (default-directory "~/downloads/youtube")
         (proc (get-buffer-process (ansi-term "/bin/bash"))))
    (term-send-string
     proc
     (concat "cd ~/downloads/youtube/tmp && youtube-dl -o '%(title)s.%(ext)s' -x --audio-format mp3 --add-metadata " str "--restrict-filenames" "\n"
             "date2name -c *.mp3" "\n"
             "mv *.mp3 ~/downloads/youtube" "\n"))))

;; unlink org mode link

(defun unlinkify ()
  "Replace an org-link with the path, or description."
  (interactive)
  (let ((eop (org-element-context)))
    (when (eq 'link (car eop))
      (message "%s" eop)
      (let* ((start (org-element-property :begin eop))
             (end (org-element-property :end eop))
             (contents-begin (org-element-property :contents-begin eop))
             (contents-end (org-element-property :contents-end eop))
             (path (org-element-property :path eop))
             (desc (and contents-begin
                        contents-end
                        (buffer-substring contents-begin contents-end))))
        (setf (buffer-substring start end) (or desc path))))))

;; time and date

(defun insert-date (arg)
  "Insert the current date in the extended ISO format.
With a prefix ARG, insert a different date string. With two prefix
arguments, also change the locale. See `format-time-string' for
possible date string replacements."
  (interactive "P")
  (if (eq (car current-prefix-arg) 16)
      (let ((system-time-locale "pt_BR"))
	(insert (format-time-string "%d/%m/%Y")))
    (if arg
	(insert (format-time-string "%d %B %Y"))
      (insert (format-time-string "%F")))))

(defun display-time-world-and-focus ()
  "Display of times in various time zones."
  (interactive)
  (display-time-world)
  (other-window 1))

;; ==================================================================
;; ˚˚ frame and window
;; ==================================================================

;; move point to the new window

(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun rotate-windows ()
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t (setq i 1)
         (setq numWindows (count-windows))
         (while (< i numWindows)
           (let* ((w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))
                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))
                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1 b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun jag/make-frame ()
  "Return a newly created frame displaying the current buffer."
  (interactive)
  (make-frame)
  (let ((frame (selected-frame)))
    (set-frame-size frame 1256 747 t)))

(defun jag/maximize-frame ()
  "Maximize frame."
  (interactive)
  (if my-default-font
      (let ((frame-size (nth 3 (assoc 2 my-fonts))))
	(eval-expression frame-size))
    (let ((frame-size (nth 3 (assoc 1 my-fonts))))
      (eval-expression frame-size))))

(defun jag/shrink-frame ()
  "Shrink frame up."
  (interactive)
  (let ((frame (selected-frame)))
    (if my-default-font
	(set-frame-size frame 1258 200 t)
      (set-frame-size frame 1256 200 t))))

(defun jag/toggle-fullscreen ()
  "Toggle full screen, time and battery mode."
  (interactive)
  (toggle-frame-fullscreen)
  (if (frame-parameter nil 'fullscreen)
      (progn
	(display-time-mode 1)
	(display-battery-mode 1))
    (display-time-mode 0)
    (display-battery-mode 0)))

(use-package test)
