;;; init.el --- Emacs configuration file. Time-stamp: <2015-11-25>

;; Copyright (c) 2012-2015 Jonathan Gregory

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
;; random as a mouse. An elegant weapon for a more civilized age.

;; package archive

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ))

(package-initialize)
(add-hook 'package-menu-mode-hook 'hl-line-mode)

;; disable tool bar, scroll bar and tool tip

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; font settings

(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Courier New-18"))
(fontify-frame nil)
(push 'fontify-frame after-make-frame-functions)

(set-face-foreground 'highlight nil)

;; disable current theme before new one is loaded

(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

;; cycle through this set of themes

(setq my-themes
      '(badger noctilux))

(setq my-cur-theme nil)
(defun cycle-my-theme ()
  "Cycle through a list of themes defined by `my-themes'"
  (interactive)
  (when my-cur-theme
    (disable-theme my-cur-theme)
    (setq my-themes (append my-themes (list my-cur-theme))))
  (setq my-cur-theme (pop my-themes))
  (load-theme my-cur-theme t)
  (set-cursor-color "gold2"))

(cycle-my-theme)
(global-set-key (kbd "C-t") 'cycle-my-theme)

;; select a different theme

(global-set-key (kbd "C-c t") 'load-theme)

;; directory and load paths

(setq user-emacs-directory (file-truename "~/.emacs.d/"))
(setq default-directory "~/Documents/org/")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq backup-directory-alist '(("." . "~/Documents/org/.backups")))
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")
(load-file "~/.emacs.d/lisp/config-gnus.el")

;; keep personal information separate

(load "~/.emacs.d/private.el" t)

;; keep custom settings separate

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; remember point position

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/lisp/saved-places")

;; default settings

(setq debug-on-error t)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq doc-view-continuous t)
(setq doc-view-resolution 80)
(setq scroll-step 1)
(setq scroll-conservatively 1000)
(setq require-final-newline t)
(setq sentence-end-double-space nil)

(global-visual-line-mode t)
(guru-global-mode +1)
(fset 'yes-or-no-p 'y-or-n-p)

;;; recenter sequence
;; C-l cycles; C-M-l moves to the top

(setq recenter-positions '(top middle bottom))

;; backup settings

(setq delete-old-versions t)
(setq delete-by-moving-to-trash t)
(setq version-control t)
(setq vc-make-backup-files t)

;; org-mode for managing notes, tasks and documents

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; scratch buffer mode and message

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message
      (concat "# GNU Emacs " emacs-version " (Org mode " org-version")\n\n"))

;; deft for browsing org files

(setq deft-extension "org")
(setq deft-directory "~/Documents/org/")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(global-set-key (kbd "\C-cd") 'deft)
(add-hook 'deft-mode-hook 'hl-line-mode)

;; helm for managing open files

(require 'helm-config)
(global-set-key (kbd "C-r") 'helm-resume)
(global-set-key (kbd "C-c s") 'helm-swoop)
(define-key org-mode-map (kbd "C-;") 'helm-org-in-buffer-headings)
(define-key isearch-mode-map (kbd "C-c S") 'helm-swoop-from-isearch)
(setq helm-swoop-split-direction 'split-window-vertically)
(setq helm-buffers-fuzzy-matching t)
(setq helm-ff-skip-boring-files t)

;; disable pre-input

(setq helm-swoop-pre-input-function
      (lambda () ""))

;; the silver searcher

(require 'helm-ag)
(global-set-key (kbd "C-c F") 'helm-ag)
(setq helm-ag-fuzzy-match t)

;; projectile for managing large projects

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-mode-line
      '(:eval
        (format " Proj[%s]"
                (projectile-project-name))))
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(setq helm-projectile-sources-list
      '(helm-source-projectile-files-list helm-source-projectile-projects))

;;; switch between buffers, files and directories
;; M-n / M-p cycles through directories
;; C-d opens a dired buffer in the current directory
;; C-z prevents ido from switching directories during file name input

(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq org-completion-use-ido t)
(setq org-outline-path-complete-in-steps nil)
(setq-default read-buffer-completion-ignore-case t)

;; flex matching

(require 'flx-ido)
(flx-ido-mode 1)

;; disable ido faces to see flx highlights

(setq ido-use-faces nil)

;; display ido prospects in a grid

(require 'ido-grid-mode)
(ido-grid-mode 1)
(setq ido-grid-mode-keys nil)
(setq ido-grid-mode-prefix-scrolls t)
(setq ido-grid-mode-prefix ">> ")

;; grid navigation

(add-hook 'ido-setup-hook
	  (lambda ()
	    (define-key ido-completion-map (kbd "C-k") #'ido-grid-mode-down)  ;;   i/p/r
	    (define-key ido-completion-map (kbd "C-i") #'ido-grid-mode-up)    ;;     |
	    (define-key ido-completion-map (kbd "C-p") #'ido-grid-mode-up)    ;; j <-+-> l
	    (define-key ido-completion-map (kbd "C-n") #'ido-grid-mode-down)  ;;     |
	    (define-key ido-completion-map (kbd "C-j") #'ido-grid-mode-left)  ;;   k/n/s
	    (define-key ido-completion-map (kbd "C-l") #'ido-grid-mode-right)
	    (define-key ido-completion-map (kbd "C-M-k") #'ido-kill-buffer-at-head)
	    (define-key ido-completion-map (kbd "C-s") #'ido-grid-mode-next)
	    (define-key ido-completion-map (kbd "C-r") #'ido-grid-mode-previous)
	    ))

;; ignore buffers, files and directories, press C-a to toggle

(add-to-list 'ido-ignore-files "\\auto/")
(add-to-list 'ido-ignore-directories "\\auto/")
(add-to-list 'ido-ignore-files "\\.log" "\\.out")
(add-to-list 'ido-ignore-files "\\.DS_Store")
(add-to-list 'ido-ignore-files "\\.backups")
(add-to-list 'ido-ignore-files "\\.Rhistory")
(add-to-list 'ido-ignore-buffers "*Completions*")

;; sort ido filelist by modification time instead of alphabetically

(ido-sort-mtime-mode 1)

;; ido sort order priority

(setq ido-file-extensions-order		;FIXME:
      '(".org" ".tex" ".html" ".pdf" ".bib"))

;; manage open buffers

(setq ibuffer-expert t
      ibuffer-show-empty-filter-groups nil)

(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("Org" (mode . org-mode))
	       ("LaTeX"
		(or
		 (mode . latex-mode)
		 (mode . bibtex-mode)
		 (mode . TeX-output-mode)
		 ))
	       ("Mail"
		(or
		 (mode . mu4e-main-mode)
		 (mode . mu4e-headers-mode)
		 (mode . mu4e-view-mode)
		 (mode . mu4e-compose-mode)
		 ))      
	       ("Emacs Lisp" (mode . emacs-lisp-mode))
	       ("Dired" (mode . dired-mode))
	       ("Shell" (name . "\*shell\*"))
	       ("Emacs"
		(or
		 (mode . Custom-mode)
		 (name . "\*Packages\*")
		 (name . "\*Compile-Log\*")
		 (name . "\*scratch\*")
		 (name . "\*Help\*")
		 (name . "\*Completions\*")
		 (name . "\*Messages\*")
		 (name . "\*Backtrace\*")
		 (name . "\*Warnings\*")))
	       ))))

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "default")
	     (ibuffer-auto-mode 1)
	     (hl-line-mode)))

;; make buffer names unique

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

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

(global-set-key (kbd "C-c R")  'rename-file-and-buffer)

;; markdown-mode

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; windmove for moving between windows

(windmove-default-keybindings)
(setq org-replace-disputed-keys t)

;; make windmove work in org-mode

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; remember recent and most frequent commands

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; aspell for spell checking

(setq-default ispell-program-name "/usr/local/bin/aspell")
(setq-default ispell-list-command "list")
(setq ispell-local-dictionary "british")
(setq ispell-extra-args '("--sug-mode=ultra"))
(global-set-key (kbd "M-4") 'ispell-word)
(global-set-key (kbd "M-5") 'ispell-region)
(global-set-key (kbd "M-6") 'ispell-buffer)

;; prevent ispell from checking code blocks and citations

(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
(add-to-list 'ispell-skip-region-alist '("^#\\+begin_example " . "#\\+end_example$"))
(add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE " . "#\\+END_EXAMPLE$"))
(add-to-list 'ispell-skip-region-alist '("\\\\cite.*{" . "}"))

;; quickly switch between dictionaries

(global-set-key [f1]
 (lambda ()
   (interactive)
   (ispell-change-dictionary "british")))

(global-set-key [f2]
 (lambda ()
   (interactive)
   (ispell-change-dictionary "brasileiro")))

;; edit and validate BibTeX files

(require 'config-bibtex)

;; RefTeX for managing citations

(setq reftex-default-bibliography '("~/Documents/org/refs.bib"))
(setq org-link-abbrev-alist
      '(("bib" . "~/Documents/org/refs.bib::%s")
        ("annotation" . "~/Documents/org/annotation.org::#%s")
        ("papers" . "~/Documents/papers/%s.pdf")))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)
(add-hook 'org-mode-hook 'org-reftex-maps)

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
         ;;enable auto-revert-mode to update reftex when bibtex file changes on disk
         (global-auto-revert-mode t)
         (reftex-parse-all)
	 )))

;; jump to entry

(defun org-mode-reftex-search ()
  "Jump to the corresponding notes used in the reftex search."
  (interactive)
  (org-open-link-from-string (format "[[annotation:%s]]" (first (reftex-citation t))))
  )

(defun org-reftex-maps ()
  ;; (define-key org-mode-map (kbd "C-c [") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c ]") 'org-mode-reftex-search))

;; org-ref compatibility issue

(if (functionp 'global-hi-lock-mode)
    (global-hi-lock-mode 1)
  (hi-lock-mode 1))

;; org-ref for managing citations

(setq org-ref-bibliography-notes "~/Documents/org/annotation.org"
      org-ref-default-bibliography '("~/Documents/org/refs.bib")
      org-ref-pdf-directory "~/Documents/papers/")
(setq org-ref-cite-onclick-function 'org-ref-cite-onclick-minibuffer-menu)
(setq org-ref-insert-cite-function 'org-ref-helm-insert-cite-link)
(setq org-ref-show-citation-on-enter nil)
(setq org-ref-colorize-links nil)
(setq org-ref-note-title-format
      "** $%a (%y) %t\n   :PROPERTIES:\n   :Custom_ID: %k\n   :END:\n")

(define-key org-mode-map (kbd "C-M-p") 'org-toggle-link-display)

;; custom open notes function

(setq org-ref-open-notes-function
      (lambda nil
	(org-show-entry)
	(org-narrow-to-subtree)
	(show-children)
	(outline-previous-visible-heading 1)
	(recenter-top-bottom 0)
	(show-children)))

(add-to-list 'load-path "~/Documents/git/org-ref")
(require 'org-ref)
(require 'jmax-bibtex)

;; helm-bibtex for managing bibliographies
;; press M-a to select all entries or C-SPC to mark entries individually

(require 'helm-bibtex)
(autoload 'helm-bibtex "helm-bibtex" "" t)
(setq helm-bibtex-bibliography "~/Documents/org/refs.bib")
(setq helm-bibtex-library-path "~/Documents/papers/")
(setq helm-bibtex-notes-path "~/Documents/org/annotation.org")
(setq helm-bibtex-full-frame nil)
(setq helm-bibtex-number-of-optional-arguments 1)
(setq helm-bibtex-cite-default-as-initial-input t)
(setq helm-bibtex-additional-search-fields '(keywords tags))
(setq helm-bibtex-notes-template-one-file
      "** $${author} (${year}) ${title}\n   :PROPERTIES:\n   :Custom_ID: ${=key=}\n   :END:\n\n")

(global-set-key (kbd "C-c C-j") 'helm-bibtex)
(define-key org-mode-map (kbd "C-c C-j") nil) ; was org-goto

;; default action

(helm-delete-action-from-source "Open PDF file (if present)" helm-source-bibtex)
(helm-add-action-to-source "Open PDF file (if present)" 'helm-bibtex-open-pdf helm-source-bibtex 0)

;; open with deafult pdf viewer

(setq helm-bibtex-pdf-open-function
      (lambda (fpath)
        (start-process "open" "*open*" "open" fpath)))

;; format citation style

(setq helm-bibtex-format-citation-functions
      '((org-mode . jag/helm-bibtex-format-citation-cite)
	(latex-mode . helm-bibtex-format-citation-cite)))

;; prompt once and use org-ref syntax

(defun jag/helm-bibtex-format-citation-cite (keys)
  "Formatter for LaTeX citation commands.  Prompts for the command and
for arguments if the commands can take any."
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
                (format "[[%s:%s][%s]]"  cite-command (s-join "," keys) pos)))
          (let ((pre (if (= helm-bibtex-number-of-optional-arguments 2)
                         (read-from-minibuffer "Prenote[1]: ") ""))
                (pos (if (= helm-bibtex-number-of-optional-arguments 2)
                         (read-from-minibuffer "Postnote[2]: ") "")))
            (if (and (= helm-bibtex-number-of-optional-arguments 2) (string= "" pre) (string= "" pos))
                (format "%s:%s" cite-command (s-join "," keys))
              (format "\\%s[%s][%s]{%s}" cite-command pre pos (s-join "," keys)))
            ))))))

(setq helm-bibtex-fallback-options 
      (quote (("Google Scholar" . "http://scholar.google.co.uk/scholar?q=%s")
              ("JSTOR" . "http://www.jstor.org/action/doBasicSearch?Query=%s")
              ("BASE" . "http://www.base-search.net/Search/Results?lookfor=%s")
              ("SciELO" . "http://search.scielo.org/?q=%s&where=ORG")
              ("Springer" . "http://link.springer.com/search?query=%s"))))

;; LaTeX exporter

(require 'ox-latex)
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

;; beamer for creating slides in LaTeX

(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

;; AUCTeX for managing (La)TeX files

(require 'latex)
(require 'tex-site)
(load "auctex.el" nil t t)
(setq TeX-parse-self t)                   ; enable parse on load
(setq TeX-auto-save t)                    ; enable parse on save
(setq TeX-PDF-mode t)                     ; instead of DVI mode
(setq reftex-plug-into-AUCTeX t)          ; RefTeX integration
(setq TeX-auto-untabify t)                ; remove tabs
(add-hook 'TeX-mode-hook 'turn-on-reftex) ; turn RefTeX on
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'auto)
(setq reftex-plug-into-AUCTeX t)

(server-start)

;; set PATH

(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
(setq exec-path (append exec-path '("/usr/texbin")))

;; use Skim as default pdf viewer
;; option -b highlights the current line; option -g opens Skim in the background  

(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))

;; doc-view needs ghostscript installed

(setq doc-view-ghostscript-program "/usr/local/Cellar/ghostscript/9.16/bin/gs")

;; automatically update when making changes

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; fix /bin/bash command not found problem

(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/usr/local/bin") exec-path))

;; LaTeX preview pane

(define-key LaTeX-mode-map (kbd "C-c u") 'latex-preview-pane-mode)

;; (latex-preview-pane-enable)
;; (add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)

;; ignore headings tagged with `ignoreheading' when exporting to LaTeX

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
                          ("\\s-*[[:upper:]]+[a-zA-Z-]+[0-9]+[a-z]" . font-lock-constant-face)))
(font-lock-add-keywords 'org-mode
                        '(("\\(\\\\citep\\)" . font-lock-keyword-face)))
(font-lock-add-keywords 'org-mode
                        '(("\\(\\\\citet\\)" . font-lock-keyword-face)))
(font-lock-add-keywords 'org-mode
                        '(("\\(\\\\citealp\\)" . font-lock-keyword-face)))
(font-lock-add-keywords 'org-mode
                        '(("\\(\\\\citeauthor\\)" . font-lock-keyword-face)))
(font-lock-add-keywords 'org-mode
                        '(("\\(\\\\citeyear\\)" . font-lock-keyword-face)))
(font-lock-add-keywords 'org-mode
                        '(("\\(\\\\citeyearpar\\)" . font-lock-keyword-face)))

;; GTD customisation

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELED(c)")
        ))

(setq org-todo-keyword-faces
      '(
        ("STARTED"  . (:foreground "SpringGreen2" :weight bold))
        ("REVISE"  . (:foreground "#E0CF9F" :weight bold))
        ("WAITING"  . (:foreground "#E0CF9F" :weight bold))
        ("DEFERRED"  . (:foreground "#E0CF9F" :weight bold))
        ))

(require 'config-mu4e)

;; org-capture templates

(setq org-capture-templates
      '(("t" "Task" entry (file+headline "~/Documents/org/todo.org" "Tasks")
         "** TODO %^{Description} %^g\n%^{Effort}p" :prepend t)
	
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
    expenses:%^{Spent on|donation:|entertainment:|food:|groceries:|home:|other:|personal:|rent:|transportation:|utilities:internet:|utilities:phone}%?  £%^{Amount}
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
         "** %^{Description}\n:PROPERTIES:\n:At: %^{At}\n:END:\n%^t\n" :prepend t :immediate-finish t)
       	
        ("h" "Habit" entry (file+headline "~/Documents/org/todo.org" "Habits")
         "** TODO %?\n   SCHEDULED: %t\n:PROPERTIES:\n:STYLE: habit\n:END:" :prepend t)

	("&" "E-mail" entry (file+headline "~/Documents/org/todo.org" "Tasks")
	 "** TODO %? %a %(jag/set-mail-tag)\n%^{Effort}p" :prepend t) ;requires org-mu4e

	("^" "E-mail appt" entry (file+headline "~/Documents/org/todo.org" "Appointments")
	 "** %?\n:PROPERTIES:\n:At: %^{At}\n:END:\n%^t\n%a" :prepend t)

	("#" "Hold" entry (file+headline "~/Documents/org/todo.org" "Tickler")
	 "** TODO delete %a %(jag/set-mail-tag)\n   SCHEDULED: %^t\n" :prepend t :immediate-finish t)

        ("F" "Fiona" entry (file+headline "~/Documents/org/orientation.org" "2015")
         "** Fiona %u\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:\n%?" :prepend t)

        ("S" "Suzel" entry (file+headline "~/Documents/org/orientation.org" "2015")
         "** Suzel %u\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:\n%?" :prepend t)))

;; fix tag alignment

(defun jag/set-mail-tag ()
  (interactive "P")
  (org-set-tags-to "mail"))

;; capture context

(setq org-capture-templates-contexts
      '(("#" ((in-mode . "mu4e-view-mode")))
	("&" ((in-mode . "mu4e-view-mode")))
	("^" ((in-mode . "mu4e-view-mode")))
	("F" ((in-file . "orientation.org")))
	("S" ((in-file . "orientation.org")))
	))

;; ditaa and plantUML for drawing diagrams

(setq org-ditaa-jar-path "~/.emacs.d/lisp/ditaa0_9/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/.emacs.d/lisp/plantuml.jar")

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

;; run ditaa inside emacs

(setq ditaa-cmd "java -jar ~/.emacs.d/lisp/ditaa0_9/ditaa0_9.jar")

(defun djcb-ditaa-generate ()
  (interactive)
  (shell-command
   (concat ditaa-cmd " " buffer-file-name)))

(defun bh/display-inline-images ()
  (condition-case t
      (org-display-inline-images)
    (error t)))

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

;; smartparens

(require 'smartparens-config)
(smartparens-global-mode t)
(load "init-smartparens")

;; expand abbreviations into templates

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets/"))
(yas-reload-all)
(add-hook 'org-mode-hook
          '(lambda ()
             (yas-minor-mode)))

;; multiple major mode support for web editing

(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; typopunct for dealing with hyphens and smart quotes

(require 'typopunct)
(typopunct-change-language 'english t)

;; auto completion

(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-auctex)
(ac-config-default)
(global-auto-complete-mode t)

(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; bookkeeping with ledger

(setq ledger-reconcile-default-commodity "£")
(setq ledger-use-iso-dates t)

;; key bindings

(global-set-key (kbd "C-o") 'ido-find-file)
(global-set-key (kbd "C-M-o") 'helm-find-files)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "M-z") 'undo-tree-undo) ; use C-x u to show tree
(global-set-key (kbd "M-r") 'undo-tree-redo)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-m") 'capitalize-word)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "C-x c") 'calendar)
(global-set-key (kbd "C-c C") 'org-contacts)
(global-set-key (kbd "C-c f") 'reveal-in-finder)
(global-set-key (kbd "C-c m") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; evaluate buffer and region

(define-key emacs-lisp-mode-map (kbd "C-c e") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c r") 'eval-region)

;; newsreader client

(require 'config-gnus)
(global-set-key (kbd "C-c g") 'gnus)

;; mail client

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'config-mu4e)
(global-set-key (kbd "M-M") 'mu4e)
(global-set-key (kbd "C-x m") 'mu4e-compose-new)

;; keep track of revisions
;; https://github.com/magit/magit/wiki/Cheatsheet

(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

;; compare file differences and merge changes

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function (quote split-window-horizontally))

(defun ora-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'ora-ediff-hook)

;; move cursor to any position in the current view

(global-set-key (kbd "C-x SPC") #'avy-goto-word-1)
(global-set-key (kbd "M-g g") #'avy-goto-line)

;; move between windows

(setq avi-keys 
      '(?a ?s ?d ?e ?f ?h ?j ?k ?l ?n ?m ?v ?r ?u))
(setq aw-keys '(?a ?s ?d ?j))
(global-set-key (kbd "C-x C-o") #'ace-window)

;; isearch with an overview

(require 'ivy)
(global-set-key (kbd "C-s") 'swiper) ; C-l recenter; M-q query-replace

;; increase selected region by semantic units

(global-set-key (kbd "C-M-3") 'er/expand-region)

;; find file in project

(autoload 'ivy-read "ivy")
(global-set-key (kbd "C-x f") 'find-file-in-project)

;; yank clipboard url as org-mode link

(define-key org-mode-map (kbd "C-x l") 'jag/insert-cliplink)

;; insert dummy text

(global-set-key (kbd "C-x i") 'Lorem-ipsum-insert-paragraphs)
(setq lorem-ipsum-sentence-separator " ")

;; highlight passive voice, duplicate words, and weasel words

(require 'writegood-mode)
(global-set-key "\C-cw" 'writegood-mode)

;; adjust font size

(global-set-key (kbd "C-M--") 'text-scale-decrease)
(global-set-key (kbd "C-M-=") 'text-scale-increase)

;; bookmarks

(global-set-key (kbd "C-1") 'bmkp-bookmark-set-confirm-overwrite)
(global-set-key (kbd "C-2") 'bookmark-bmenu-list)
(global-set-key (kbd "C-3") 'bookmark-jump)
(global-set-key (kbd "C-4") 'headlong-bookmark-jump)

;; frame and window

(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below-and-move-there-dammit)
(global-set-key (kbd "M-3") 'split-window-right-and-move-there-dammit)
(global-set-key (kbd "C-c C-x r") 'rotate-windows)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-)") 'delete-frame) ; was move-past-close-and-reindent

;; avoid backspace and return keys; use C-m or C-j instead

;; (global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-delete-word) ; use C-DEL to backward-kill-word
(global-set-key (kbd "M-d") 'delete-word)          ; instead of kill-word
(global-set-key (kbd "M-DEL") 'backward-delete-word)
(define-key org-mode-map (kbd "M-h") nil)
(global-set-key (kbd "M-j") 'delete-backward-char) ; was indent-new-comment-line
(global-set-key (kbd "µ") 'indent-new-comment-line) ; that's alt-m
(define-key org-mode-map (kbd "C-j") 'org-return) ; instead of org-return-indent

;; help commands

(global-set-key (kbd "C-h h") 'helm-apropos)

;; avoid arrow keys when promoting and demoting lists

(define-key org-mode-map (kbd "∆") 'org-metaup)		; alt-j
(define-key org-mode-map (kbd "˚") 'org-metadown)	; alt-k
(define-key org-mode-map (kbd "˙") 'org-shiftmetaleft)	; alt-h
(define-key org-mode-map (kbd "¬") 'org-shiftmetaright) ; alt-l

(define-key org-mode-map (kbd "≤") 'org-shiftleft)	; alt-,
(define-key org-mode-map (kbd "≥") 'org-shiftright)	; alt-.

;;; transpose words, sentences and paragraphs
;; use M-t to transpose-words and C-x C-t to transpose-sentences; use
;; alt-j / alt-k to transpose paragraphs in org-mode

(global-set-key (kbd "C-x C-t") 'transpose-sentences) ;was transpose-lines

;; info mode navigation

(define-key Info-mode-map (kbd "SPC") (lambda ()
					(interactive)
					(scroll-up-1 4)))

(define-key Info-mode-map (kbd "k") (lambda ()
				      (interactive)
				      (scroll-down-1 4))) ;FIXME: keychord conflict

;; help mode navigation

(add-hook 'help-mode-hook
	  (lambda ()
	    (define-key help-mode-map "l" 'help-go-back)))

;; unbind

(define-key ibuffer-mode-map (kbd "M-o") nil)
(define-key ibuffer-mode-map (kbd "C-o") nil)
(define-key dired-mode-map (kbd "C-o") nil) ; use C-m instead
(define-key yas-minor-mode-map (kbd "C-c &") nil) ; org-mark-ring-goto
(define-key org-mode-map (kbd "C-'") nil) ; org-cycle-agenda-files
(define-key org-mode-map (kbd "C-,") nil) ; org-cycle-agenda-files
(define-key org-mode-map (kbd "M-p") nil) ; org-shiftup
(define-key org-mode-map (kbd "C-c [") nil) ; org-agenda-file-to-front

;; another option for moving to the previous line

(define-key org-mode-map (kbd "M-n") nil)   ; was org-shiftdown
(global-set-key (kbd "M-n") 'previous-line)
(define-key org-mode-map (kbd "M-N") 'org-shiftdown)

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (local-set-key (kbd "M-n") 'previous-line))) ; was
                                                         ; org-agenda-priority-down

(add-hook 'magit-status-mode-hook
          (lambda ()
            (local-set-key (kbd "M-n") 'previous-line)))

;; avoid arrow keys when switching buffers

(global-set-key (kbd "C-x .") 'next-buffer) ; consider M-n
(global-set-key (kbd "C-x ,") 'previous-buffer) ; consider M-p
(global-set-key (kbd "M-˚") 'next-buffer) ; that's M-alt-k, similar to switching tabs in ff
(global-set-key (kbd "M-∆") 'previous-buffer) ; that's M-alt-j

;; use command as meta key and option for dead keys

(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; key chord

(key-chord-mode 1)
(key-chord-define-global "xc" 'ido-dired)
(key-chord-define-global "jk" "Cape Town")
(key-chord-define-global "jl" "South Africa")
(key-chord-define-global "jj" (lambda() (interactive)(find-file "~/Documents/org/todo.org")))
(key-chord-define-global "kk" (lambda() (interactive)(find-file "~/Documents/org/notes.org")))
(key-chord-define-global "hh" (lambda() (interactive)(find-file "~/Documents/org/fieldwork.org")))

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
(key-chord-define-global "p[" "{")

;; add prefix key

(define-prefix-command 'jag-key-map)

(global-set-key (kbd "M-i") 'jag-key-map)
(define-key jag-key-map "i" #'jag/sr-speedbar-toggle)
(define-key jag-key-map "c" #'calculator)
(define-key jag-key-map "a" #'bbdb)
(define-key jag-key-map "b" #'boxquote-region)
(define-key jag-key-map "u" #'boxquote-unbox)
(define-key jag-key-map (kbd "M-q") #'unfill-paragraph)
(define-key jag-key-map "t" #'git-timemachine)
(define-key jag-key-map "T" #'git-timemachine-toggle)

;; quickly switch between dictionaries

(define-key jag-key-map "e" (lambda ()
			     (interactive)
			     (ispell-change-dictionary "british")))
(define-key jag-key-map "p" (lambda ()
			     (interactive)
			     (ispell-change-dictionary "brasileiro")))

;; guide the following key bindings

(guide-key-mode 1)
(setq guide-key/popup-window-position (quote bottom))
(setq guide-key/guide-key-sequence '("M-p"   ; mplayer/emms
                                     "M-i"
				     "M-g"
				     "C-h"
                                     "C-c p"
                                     "C-c i"
                                     "C-x r"
                                     "C-x p"
                                     "C-x j"))

;; default start-up frame

(let ((frame (selected-frame)))
  (set-frame-size frame 1254 747 t))

;; enlarge frame

(defun jag/toggle-max-frame ()
  "Toggle maximization state of the selected frame."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-size frame 1254 747 t)))

(global-set-key (kbd "C-7") 'jag/toggle-max-frame)
(global-set-key (kbd "C-6") 'toggle-frame-fullscreen) ; toggle fullscreen

;; fix frame for presentations

(defun jag/presentation-frame ()
  "Hide menu bar and adjust frame size when making presentations."
  (interactive)
  (setq ns-auto-hide-menu-bar t)
  (set-frame-position nil 0 0)
  (set-frame-size nil 114 38))

(global-set-key (kbd "<f7>") 'jag/presentation-frame)

;; toggle frame size

(defun jag/custom-frame ()
  "Shrink frame up."
  (interactive)
  (set-frame-size nil 114 10))

(global-set-key (kbd "C-x <up>") 'jag/custom-frame)
(global-set-key (kbd "C-x <down>") 'jag/toggle-max-frame)

;; open a new frame

(defun jag/open-new-frame ()
  "Similar to `make-frame' but with custom size."
  (interactive)
  (make-frame)
  (let ((frame (selected-frame)))
    (set-frame-size frame 1254 747 t)))

(global-set-key (kbd "C-c n") 'jag/open-new-frame)

;; load files automatically

(require 'org-checklist)
(require 'org-collector)
(require 'org-habit)
(require 'org-id)
(require 'ox-org)
(require 'ox-beamer)
(require 'org-mouse)
;; (require 'ox-bibtex)
;; (require 'org-contacts)
;; (require 'ox-odt)

;; enable multimedia support

(require 'config-emms)

;; search online

(require 'config-queries)

;; bookmark+

(require 'bookmark+)
(setq bookmark-completion-ignore-case nil)
(bookmark-maybe-load-default-file)
(setq bookmark-save-flag 1)

;; smooth and in place scrolling

(require 'smooth-scroll)
(smooth-scroll-mode t)
(global-set-key (kbd "C-M-k") (lambda () (interactive) (scroll-up-1 4)))
(global-set-key (kbd "C-M-j") (lambda () (interactive) (scroll-down-1 4)))

(defhydra hydra-scroll (:hint nil
                              :pre (smooth-scroll-mode 0)
                              :post (smooth-scroll-mode t))
  "
 _SPC_↑↓_k_     _q_uit
" 
  ("j" (lambda () (interactive) (scroll-up-1 4)))
  ("k" (lambda () (interactive) (scroll-down-1 4)))
  ("SPC" (lambda () (interactive) (scroll-up-1 4)))

  ("C-j" (lambda () (interactive) (scroll-up-1)))
  ("C-k" (lambda () (interactive) (scroll-down-1)))
  ("C-SPC" (lambda () (interactive) (scroll-up-1)))

  ("i" scroll-up)
  ("o" scroll-down)

  ("," beginning-of-buffer)
  ("." end-of-buffer)
  
  ("C-l" recenter-top-bottom)
  ("l" nil)
  ("q" nil))

(global-set-key (kbd "C-M-SPC") 'hydra-scroll/body)
(global-set-key (kbd "C-v") 'jag/scroll-other-window)
(global-set-key (kbd "C-M-v") 'jag/scroll-other-window-down)

(defun jag/scroll-other-window ()
  (interactive)
  (smooth-scroll/scroll-other-window 1))

 (defun jag/scroll-other-window-down ()
  (interactive)
  (smooth-scroll/scroll-other-window-down 1))

;; org-mode settings

(setq org-confirm-babel-evaluate nil)
(setq org-tags-column -50)
(setq org-reverse-note-order t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(setq org-log-into-drawer t)
(setq org-return-follows-link nil) 	;FIXME:
(setq org-cycle-global-at-bob t)
(setq org-id-method (quote uuidgen))
(setq org-id-link-to-org-use-id
      'create-if-interactive-and-no-custom-id) ; C-c l/C-c C-l
(setq org-habit-graph-column 55)
(setq org-habit-preceding-days 14)
(setq org-clock-out-remove-zero-time-clocks t)

(setq org-columns-default-format "%50ITEM(Task) %6Effort(Effort Estimate){:} %6CLOCKSUM(Actual Time)")
(setq org-global-properties (quote (("Effort_ALL" . "0:25 0:02 0:05 0:10 0:15 0:20 0:40 1:00 2:00 4:00")
                                    ("STYLE_ALL" . "habit"))))

;; archive and refile entries

(setq org-archive-location "%s_archive::* Archived Tasks")
(setq org-archive-reversed-order t)
(setq org-refile-targets (quote ((nil :maxlevel . 4)
                                 (org-agenda-files :maxlevel . 4))))

;;; renumber footnotes when new ones are inserted
;; press C-c C-x f to insert a new reference and C-u C-c C-x f for a
;; list of options. Note that calling org-edit-footnote-reference (C-c
;; ') allows editing its definition.

(setq org-footnote-auto-adjust t)

;; org export settings

(setq org-export-with-tags 'not-in-toc)
(setq org-export-with-smart-quotes t)
(setq org-export-with-todo-keywords nil)

;; pomodoro technique

(require 'org-pomodoro)
(setq org-pomodoro-long-break-frequency 4)
(setq org-pomodoro-long-break-length 20)
(setq org-pomodoro-expiry-time 180)
(setq org-pomodoro-audio-player "mplayer")
(setq org-pomodoro-finished-sound-args "-volume 0.3")
(setq org-pomodoro-long-break-sound-args "-volume 0.3")
(setq org-pomodoro-short-break-sound-args "-volume 0.3")
(global-set-key (kbd "∏") 'org-pomodoro) ; that's S-alt-p

;; agenda settings

(setq org-agenda-files (quote ("~/Documents/org/todo.org"
                               "~/Documents/org/notes.org"
                               "~/Documents/org/fieldwork.org"
			       "~/Documents/org/analysis.org"
                               ;;"~/Documents/org/annotation.org"
                               ;;"~/Documents/org/qda.org"
                               ;;"~/Documents/org/draft.org"
                               "~/Documents/org/contacts.org")))
(setq org-agenda-remove-tags t)
(setq org-agenda-skip-function
      '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELED" "DEFERRED")))
(setq org-deadline-warning-days 5)
(setq org-agenda-show-log t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-tags-exclude-from-inheritance '("project"))
(setq org-log-done 'time)
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-use-time-grid nil)
(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;; extend current day to 1 hour after midnight

(setq org-extend-today-until 1)

;; display property drawer content in the agenda

(setq org-agenda-property-list
      (quote ("ADDRESS" "LOCATION" "At" "PHONE")
             ))

;; clock report parameters

(setq org-agenda-clockreport-parameter-plist ;FIXME: agenda-with-archives has no effect
      (quote (:maxlevel 3 :scope agenda-with-archives :block thisweek :compact t :fileskip0 t :properties
			("Effort" "Difference" "Pomodoro") :formula "$3=$6-$2;T::$4=($6/25)*60;t")))

(setq org-clock-clocktable-default-properties ;FIXME: :scope doesn't update
      (quote (:maxlevel 3 :scope file :scope file :compact t :fileskip0 t :properties
			("Effort" "Difference" "Pomodoro") :formula "$2=$5-$1;T::$3=($5/25)*60;t")))

;; FIXME: (setq org-pretty-entities t) ; C-c a a R table alignment issue
;; temporary fix http://is.gd/Z2qHZj

(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "\\"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "--")))
      (concat str "__ "))))

(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)

;; custom agenda views

(require 'config-agenda)

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
(define-key org-agenda-mode-map "i" 'org-agenda-clock-in)
(define-key org-agenda-mode-map "o" 'org-agenda-clock-out)

;; publish org-mode files to html

(require 'ox-publish)
(global-set-key (kbd "M-P") 'jag/org-publish-current-file)
(global-set-key (kbd "M-i P") 'org-publish-current-project)

(defun jag/org-publish-current-file (&optional force async)
  "Publish the current file.
With prefix argument FORCE, force publish the file.  When
optional argument ASYNC is non-nil, publishing will be done
asynchronously, in another process."
  (interactive "P")
  (save-excursion			;restore point position
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

(setq org-publish-project-alist
      '(("org"
         :base-directory "~/Documents/web"
         :base-extension "org"
         :publishing-directory "~/Documents/web"
         :publishing-function org-html-publish-to-html
         :recursive t
         :headline-levels 3
         :auto-preamble t)

        ("website" :components ("org"))))

;; statistical programming

(require 'ess-site)
(setq ess-eval-visibly-p nil)
(setq ess-ask-for-ess-directory nil)
(show-paren-mode 1)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.r$" . R-mode))

;; LilyPond for writing music scores

(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
(setenv "PATH" (concat "/Applications/LilyPond.app/Contents/Resources/bin:/usr/bin" (getenv "PATH") ))
(push "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp" load-path)

(eval-after-load "LilyPond-mode"
  '(progn
     (load-library "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp/ac-lilypond.el")
     (define-key LilyPond-mode-map [C-tab] 'LilyPond-autocompletion)))

;; press C-c C-s to view pdf

(setq LilyPond-pdf-command "open -a 'Skim'")

;; music programming with overtone

(add-to-list 'load-path "~/.emacs.d/scel/el")
(require 'sclang)
(require 'cider)
(add-hook 'cider-mode-hook #'eldoc-mode)

(setenv "PATH" (concat (getenv "PATH") ":/Applications/SuperCollider:/Applications/SuperCollider/SuperCollider.app/Contents/Resources"))
(setq exec-path (append exec-path '("/Applications/SuperCollider"  "/Applications/SuperCollider/SuperCollider.app/Contents/Resources" )))

;; insert random uuid

(defun insert-random-uuid ()
  "Insert a random UUID. Example of a UUID: 1df63142"
  (interactive)
  (insert
   (format "%04x%04x"
           (random (expt 16 4))
           (random (expt 16 6)))))

(defun insert-random-number ()
  "Insert a random number between 0 to 999999."
  (interactive)
  (insert (number-to-string (random 999999))) )

;; kill buffer and its windows

(substitute-key-definition 'kill-buffer
                           'kill-buffer-and-its-windows
                           global-map)

;; enable encryption

(require 'epa-file)

;; encrypt org-mode entries

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

;; delete line but don't kill it.
;; to kill it, use kill-whole-line (C-S-backspace) instead.
;; to kill the sentence, use kill-sentence (M-k).

(defun delete-line-no-kill ()
  "Delete line but don't kill it."
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1))

(global-set-key (kbd "C-k") 'delete-line-no-kill)
(define-key org-mode-map (kbd "C-k") 'delete-line-no-kill)

;; toggle inline images

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-M-i") 'org-toggle-inline-images)))

(add-hook 'org-babel-after-execute-hook
          (lambda () (org-display-inline-images nil t)))

;; plot tables in org-mode

(require 'gnuplot-mode)
(setq gnuplot-flags "-persist -pointsize 2")

;; update timestamp when file is saved

(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-format "%04y-%02m-%02d")

;; dired for managing directories

(require 'dired-x)
(setq dired-omit-mode t)
(setq dired-omit-size-limit nil)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))
(setq dired-dwim-target t)   ; guess default target directory when
                             ; copying or renaming files
(setq dired-omit-files (concat dired-omit-files ; C-x M-o to toggle
                               "\\|^#"
                               "\\|.DS_Store$"
                               "\\|.backups$"
                               "\\|.localized$"
                               "\\|.Rhistory$"))

(add-hook
 'dired-mode-hook
 (lambda()
   (define-key dired-mode-map "j" 'swiper)))

;; reload dired buffer after making changes

(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
  (eval `(defadvice ,it (after revert-buffer activate)
           (revert-buffer))))

;; preview images in dired using qlmanage
;; see `image-dired' for more information

(define-key dired-mode-map (kbd "<SPC>") (lambda () (interactive)
					   (start-process "preview" nil "qlmanage" "-p" (dired-get-file-for-visit))))

;; open file using default application

(defun xah-open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app.

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

(global-set-key (kbd "C-c <C-return>") 'xah-open-in-external-app)

;; manage directories as trees

(setq speedbar-use-images nil)
(setq sr-speedbar-delete-windows t)
(setq sr-speedbar-auto-refresh nil)
(setq speedbar-show-unknown-files t)

(add-hook 'speedbar-mode-hook
	  '(lambda ()
	     (define-key speedbar-mode-map (kbd "k") 'speedbar-next)
	     (define-key speedbar-mode-map (kbd "i") 'speedbar-prev)
	     (define-key speedbar-mode-map (kbd "l") 'speedbar-edit-line)
	     (define-key speedbar-mode-map (kbd "C-m") 'jag/speedbar-edit-line)
	     (define-key speedbar-mode-map (kbd "q") 'kill-this-buffer)))

(add-hook 'speedbar-mode-hook 'hl-line-mode)

(defun disable-key-chord-mode ()
  (set (make-local-variable 'input-method-function) nil))

(add-hook 'speedbar-mode-hook #'disable-key-chord-mode)

(defun jag/sr-speedbar-toggle ()
  (interactive)
  (cd "~/Documents/")
  (sr-speedbar-toggle)
  (other-window 1)
  (beginning-of-buffer))

(defun jag/speedbar-edit-line ()
  (interactive)
  (speedbar-edit-line)
  (jag/sr-speedbar-toggle))

;; timer

(require 'tea-time)
(setq tea-time-sound "~/Documents/archive/audio/bell.wav")
(setq tea-time-sound-command "mplayer -volume 0.5 %s")
(add-to-list 'org-speed-commands-user '("I" call-interactively 'tea-time))
(global-set-key (kbd "C-c C-x t") 'tea-time)

;; unfill paragraph undoes M-q

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;; mark task as done in the agenda buffer

(defun jag/org-agenda-done ()
  "Mark item at point as done only if it is not scheduled to
repeat."
  (interactive)
  (save-excursion
      (org-agenda-switch-to)
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (if (re-search-forward ":repeat:" nil 'noerror)
	  (message "This item repeats. Press \"E\" to check or \":\" to remove restriction.")
	(progn
	    (switch-to-buffer "*Org Agenda*")
	    (org-agenda-todo "DONE"))
	)
      (widen)
      (switch-to-buffer "*Org Agenda*")))

(define-key org-agenda-mode-map "d" 'jag/org-agenda-done)

(defun sacha/org-agenda-next (&optional arg)
  "Mark current TODO as NEXT. This changes the line at point, all
other lines in the agenda referring to the same tree node, and
the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "NEXT"))
(define-key org-agenda-mode-map "X" 'sacha/org-agenda-next)

;; org tree slide for making presentations

(when (require 'org-tree-slide nil t)
  (global-set-key (kbd "<f8>") 'org-tree-slide-mode)
  (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle))

(define-key org-tree-slide-mode-map (kbd "<right>") 'org-tree-slide-move-next-tree)
(define-key org-tree-slide-mode-map (kbd "<left>") 'org-tree-slide-move-previous-tree)
(setq org-tree-slide-slide-in-effect nil)
(setq org-tree-slide-cursor-init nil)

;; frame and window

(defun kill-buffer-and-its-frame ()
  "Kill the current buffer as well as its frame."
  (interactive)
  (kill-buffer)
  (quit-window)
  (delete-frame))

(global-set-key (kbd "C-c 0") 'kill-buffer-and-its-frame)

(defun close-and-kill-next-pane ()
  "If there are multiple windows, then close the other pane and
kill the buffer in it also."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(global-set-key (kbd "C-c k") 'close-and-kill-next-pane)

;; move point to the new window

(defun split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun split-window-below-and-move-there-dammit ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defun rotate-windows ()
  "Rotate your windows."
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))
                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))
                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1 b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

;; export org headings to a separate file
;; http://is.gd/jYpEVC

(defun org-export-all (backend)
  "Export all subtrees that are *not* tagged with :noexport: to
separate files.

Note that subtrees must have the :EXPORT_FILE_NAME: property set
to a unique value for this to work properly."
  (interactive "sEnter backend: ")
  (let ((fn (cond ((equal backend "html") 'org-html-export-to-html)
                  ((equal backend "latex") 'org-latex-export-to-latex)
                  ((equal backend "pdf") 'org-latex-export-to-pdf)
		  ((equal backend "ascii") 'org-ascii-export-to-ascii))))
    (org-map-entries (lambda () (funcall fn nil t)) "-noexport")))

;; add table of contents in org-mode

(if (require 'toc-org nil t)
    (add-hook 'org-mode-hook 'toc-org-enable)
  (warn "toc-org not found"))

;; insert date

(defun jag/insert-iso-date ()
  "Insert the current date at point. See `format-time-string' for
possible date string replacements."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun jag/insert-date ()
  "Insert the current date at point. See `format-time-string' for
possible date string replacements."
  (interactive)
  (insert (format-time-string "%d %b %Y")))

(global-set-key (kbd "C-c i i") 'jag/insert-iso-date)
(global-set-key (kbd "C-c i d") 'jag/insert-date)

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

;; delete word without killing it
;; http://www.emacswiki.org/emacs/BackwardDeleteWord

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

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

;; wrap text with punctation

(wrap-region-mode t)
(add-hook 'org-mode-hook 'wrap-region-mode)

(wrap-region-add-wrapper "*" "*" nil 'org-mode)
(wrap-region-add-wrapper "/" "/" nil 'org-mode)
(wrap-region-add-wrapper "_" "_" nil 'org-mode)
(wrap-region-add-wrapper "=" "=" nil 'org-mode)
(wrap-region-add-wrapper "+" "+" nil 'org-mode)
(wrap-region-add-wrapper "~" "~" nil 'org-mode)

;; unclutter the modeline

(require 'diminish)

(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "wrap-region" '(diminish 'wrap-region-mode))
(eval-after-load "guide-key" '(diminish 'guide-key-mode))
(eval-after-load "smooth-scroll" '(diminish 'smooth-scroll-mode))
(eval-after-load "real-auto-save" '(diminish 'real-auto-save-mode))
(eval-after-load "guru-mode" '(diminish 'guru-mode))

(diminish 'auto-fill-function)
(diminish 'visual-line-mode)
(diminish 'auto-complete-mode)
(diminish 'hi-lock-mode)

;; mode-line clock face is a visual reminder of the running clock

(set-face-attribute 'org-mode-line-clock nil
                    :background "darkred"
                    :foreground "grey90"
                    :inherit nil)

;; treat undo history as tree using C-x u

(global-undo-tree-mode)
(setq undo-tree-mode-lighter "")

;; browse kill-ring list

(global-set-key "\C-cy" 'browse-kill-ring)
(setq browse-kill-ring-highlight-current-entry t
      browse-kill-ring-display-duplicates nil)

;; move line or region using M-up M-down

(move-text-default-bindings)

;; move back and forth between places in the buffer
;; C-SPC C-SPC to mark, C-u C-SPC to jump back

(setq mark-ring-max 1)
(setq set-mark-command-repeat-pop t)

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

(global-set-key (kbd "C-M-e") 'jag/forward-paragraph)
(global-set-key (kbd "C-M-a") 'jag/backward-paragraph)

;; transcribe audio

(require 'transcribe-mode)
(setq transcribe-interviewer "Jonathan")
(setq transcribe-interviewee "Interviewee")

;; dictionary, thesaurus and translation tools
;; inspired by https://github.com/jkitchin/jmax/blob/master/words.el

(key-chord-define-global
 "ww"
 (defhydra hydra-words (:color teal :hint nil)
   "
^Dictionary^               ^Thesaurus^             ^Translation^             ^Other
--------------------------------------------------------------------------------------------
  _d_: dictionary          _t_: thesaurus          _e_: translate to en      _g_: grove music online
  _s_: search word         _h_: oxford thesaurus   _P_: translate to pt      _o_: other dictionaries
  _a_: portuguese          _T_: search online      _E_: open browser         _w_: wikipedia
_M-s_: search online       ^ ^                     _P_: open browser         _q_: quit
"
   ("d" osx-dictionary-search-pointer) ; see osx-dictionary-search-log-file
   ("s" osx-dictionary-search-input)
   ("a" osx-dictionary-second-dictionary-search-input)
   ("M-s" dictionary-search)

   ("t" thesaurus)
   ("h" osx-dictionary-thesaurus-search-input)
   ("T" thesaurus-search)

   ("e" google-translate-at-point)
   ("p" google-translate-at-point-reverse)
   ("E" translate-to-en)
   ("P" translate-to-pt)

   ("g" grove-music-online)
   ("o" helm-dictionary)
   ("w" wiki-summary)
   ("q" nil)))

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
  (osx-dictionary-search-pointer))

(defun grove-music-online ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (eww-open-file "~/Documents/archive/grove/TOC.htm"))

;; additional dictionaries

(setq helm-dictionary-database "~/Library/Spelling/LocalDictionary")
(setq helm-dictionary-online-dicts
      '(("linguee" . "http://www.linguee.com/english-portuguese/search?source=auto&query=%s")
        ("dicio.com.br" . "http://www.dicio.com.br/%s/")
        ("en.wiktionary.org" . "http://en.wiktionary.org/wiki/%s")
        ("pt.wiktionary.org" . "http://pt.wiktionary.org/wiki/%s")
        ("sinonimos.com.br" . "http://www.sinonimos.com.br/%s/")))

;; thesaurus

(require 'synonyms)
(setq synonyms-file "~/Documents/archive/mthesaur.txt")
(setq synonyms-cache-file "~/.emacs.d/mthesaur.cache")

(defun thesaurus ()
  (interactive)
  (synonyms)
  (windmove-down))

(defun thesaurus-search ()
  "Look up word at point in an online thesaurus."
  (interactive)
  (browse-url
   (format
    "http://www.thesaurus.com/browse/%s"
    (thing-at-point 'word))))

;; translation

(setq google-translate-default-source-language "pt")
(setq google-translate-default-target-language "en")
(setq google-translate-pop-up-buffer-set-focus t)
(setq google-translate-translation-directions-alist
      '(("pt" . "en") ("en" . "pt")))

(defun translate-to-pt ()
  "Translate from English to Portuguese by querying word at
point. If region is active, use that instead."
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
  "Translate from Portuguese to English by querying word at
point. If region is active, use that instead."
  (interactive)
  (browse-url
   (format
    "https://translate.google.com/#pt/en/%s"
    (if (use-region-p)
        (url-hexify-string (buffer-substring
                            (region-beginning)
                            (region-end)))
      (thing-at-point 'word)))))

;; bookmark web pages as org-mode link

(defun jag/insert-cliplink ()
  (interactive)
  (org-cliplink-retrieve-title
   (substring-no-properties (current-kill 0))
   '(lambda (url title)
      (insert (concat "[[" url "][" title "]]")))))

;; save buffer modifications automatically

(require 'real-auto-save)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 5)

;; highlight comment annotations

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|TODO\\):" 1 font-lock-warning-face t)))))

;; comment and copy with C-u M-;
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

;;; download and watch youtube videos from emacs
;; http://oremacs.com/2015/01/05/youtube-dl/
;; https://github.com/rg3/youtube-dl

(defun youtube-to-mp4 ()
  "Download youtube video to disk as an mp4 file using youtube-dl
and append a date to it using date2name."
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
  "Download youtube video to disk as an mp3 file using youtube-dl
and append a date to it using date2name."
  (interactive)
  (let* ((str (current-kill 0))
         (default-directory "~/downloads/youtube")
         (proc (get-buffer-process (ansi-term "/bin/bash"))))
    (term-send-string
     proc
     (concat "cd ~/downloads/youtube/tmp && youtube-dl -o '%(title)s.%(ext)s' -x --audio-format mp3 --add-metadata " str "--restrict-filenames" "\n"
	     "date2name -c *.mp3" "\n"
	     "mv *.mp3 ~/downloads/youtube" "\n"))))

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

;; edit files in the YAML data serialization format

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; meditation timer

(add-to-list 'load-path "~/Documents/git/org-meditation")
(require 'org-meditation)
(define-key org-agenda-mode-map "1" 'org-meditation)

(require 'test)

