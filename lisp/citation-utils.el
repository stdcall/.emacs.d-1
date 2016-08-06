;;; citation-utils.el --- Citation utilities for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Code here is experimental.

;;; Code:

(require 'org-ref)
(require 'helm-bibtex)

(defcustom power-ref-actions
  '(("Open PDF        `C-M-p'"  . helm-bibtex-open-pdf)
    ("Insert citation `C-M-c'"  . helm-bibtex-insert-citation)
    ("Edit notes      `C-M-n'"  . bibtex-completion-edit-notes)
    ("Add keywords    `C-M-k'"  . power-ref-tag-entries)
    ("Show entry      `C-M-e'"  . bibtex-completion-show-entry)
    ("Annotate        `C-M-a'"  . power-ref-annotate)
    ("Insert notes template"    . power-ref-insert-notes-template)
    ("Open URL or DOI"          . helm-bibtex-open-url-or-doi)
    ("Insert reference"         . helm-bibtex-insert-reference)
    ("Attach PDF to email"      . helm-bibtex-add-PDF-attachment))
  "Cons cells of string and function to set the actions of `helm-bibtex' to.
The car of cons cell is the string describing the function. The cdr of
the the cons cell is the function to use."
  :type '(alist :key-type string :value-type function))

(defcustom power-ref-number-of-optional-arguments 1
  "The number of optional arguments (aka pre and postnotes)."
  :type 'integer)

(defconst power-ref-preferred-number-of-optional-arguments
  power-ref-number-of-optional-arguments)

(defun power-ref-citation-format (keys)
  "Formatter for `org-ref' citation commands.
Prompt for the command and additional arguments if the commands can
take any. To use this formatter, add it to
`bibtex-completion-format-citation-functions'."
  (let* ((initial (when bibtex-completion-cite-default-as-initial-input bibtex-completion-cite-default-command))
         (default (unless bibtex-completion-cite-default-as-initial-input bibtex-completion-cite-default-command))
         (default-info (if default (format " (default \"%s\")" default) ""))
	 ;; (cite-command (helm-comp-read
         ;;                (format "Cite command%s: " default-info)
         ;;                bibtex-completion-cite-commands :must-match nil :initial-input initial)))
	 (cite-command (completing-read (format "Cite command%s: " default-info)
	 				bibtex-completion-cite-commands nil nil initial
	 				'bibtex-completion-cite-command-history default nil)))
    (if (member cite-command '("nocite" "supercite"))  ; These don't want arguments.
        (format "%s:%s" cite-command (s-join "," keys))

      (if (= power-ref-number-of-optional-arguments 0)
          (format "%s:%s" cite-command (s-join "," keys))

        (if (= power-ref-number-of-optional-arguments 1)
            (let ((pos (if (= power-ref-number-of-optional-arguments 1)
                           (read-from-minibuffer "Postnote[1]: ") "")))
              (if (and (= power-ref-number-of-optional-arguments 1) (string= "" pos))
                  (format "%s:%s" cite-command (s-join "," keys))
                (format "[[%s:%s][%s]]" cite-command (s-join "," keys) pos)))

          (let ((pre (if (= power-ref-number-of-optional-arguments 2)
                         (read-from-minibuffer "Prenote[1]: ") ""))
                (pos (if (= power-ref-number-of-optional-arguments 2)
                         (read-from-minibuffer "Postnote[2]: ") "")))
            (if (and (= power-ref-number-of-optional-arguments 2) (string= "" pre) (string= "" pos))
                (format "%s:%s" cite-command (s-join "," keys))
              (format "[[%s:%s][%s::%s]]" cite-command (s-join "," keys) pre pos))))))))

;; small alignment changes

(defun bibtex-completion-candidates-formatter (candidates width)
  "Formats BibTeX entries for display in results list.
Adapted from the function in `bibtex-completion' to include additional
keywords field."
  (cl-loop
   with width = (with-helm-window (helm-bibtex-window-width))
   for entry in candidates
   for entry = (cdr entry)
   for entry-key = (bibtex-completion-get-value "=key=" entry)
   if (assoc-string "author" entry 'case-fold)
   for fields = '("author" "title"  "year" "=has-pdf=" "=has-note=" "=type=")
   else
   for fields = '("editor" "title" "year" "=has-pdf=" "=has-note=" "=type=")
   for fields = (--map (bibtex-completion-clean-string
                        (bibtex-completion-get-value it entry " "))
                       fields)
   for fields = (-update-at 0 'bibtex-completion-shorten-authors fields)
   for fields = (append fields
                        (list (or (bibtex-completion-get-value "keywords" entry)
                                  "")))
   collect
   (cons (s-format "$2 $0 $1 $3 $4 $5 $6" 'elt
                   (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s))
                              fields (list 25 (- width 60) 4 1 1 7 14)))
         entry-key)))

;; unescape characters

(defun bibtex-completion-clean-string (s)
  "Removes quoting and superfluous white space from BibTeX field
values."
  (let* ((s (replace-regexp-in-string "[\n\t ]+" " " s))
	 (s (replace-regexp-in-string "\\\\&" "&" s))
	 (s (replace-regexp-in-string "[\\\.{}]+" "" s)))
    (if s s nil)))

(defun power-ref-insert-notes-template (_)
  "Insert notes template associated with key."
  (let* ((keys (helm-marked-candidates :with-wildcard t))
	 (entry (bibtex-completion-get-entry (car keys))))
    (with-helm-current-buffer
      (insert (s-format bibtex-completion-notes-template-one-file
			'bibtex-completion-apa-get-value
			entry)))))

(defun power-ref-annotate (_candidate)
  "Prepare entry for annotation."
  (let* ((key (helm-marked-candidates))
	 (entry (bibtex-completion-get-entry (car key)))
	 (pdf (car (bibtex-completion-find-pdf (car key)))))
    (if pdf
	(progn
	  (split-window-below)
	  (find-file pdf)
	  (bibtex-completion-edit-notes (car key)))
      (bibtex-completion-edit-notes (car key)))))

(defun power-ref-insert-figure ()
  "Insert figure, caption and label at point."
  (interactive)
  (progn
    (org-insert-link '(4))
    (org-cycle)
    (org-beginning-of-line)
    (split-line 1)
    (insert (concat "\n#+caption: "
		    (read-string "caption: ")
		    " "))
    (org-cycle)
    (org-ref-helm-insert-label-link)))

;; ==================================================================
;; ˚˚ local database
;; ==================================================================

(defun bibtex-completion--get-local-databases ()
  "Return a list of .bib files associated with the current file.
This function searches the current file or its master file for a
`\\bibliography' or `\\addbibresource' command and returns the
associated .bib file(s). If no files are found locally, return the
files specified in the variable `bibtex-completion-bibliography'."
  (let ((texfile nil)
	(dir (file-name-directory (buffer-file-name)))
	(cb (current-buffer)))
    (when (and (boundp 'TeX-master)
	       (stringp TeX-master))
      (setq texfile (if (file-name-extension TeX-master)
			TeX-master
		      (concat TeX-master ".tex"))))
    (with-temp-buffer
      (if (and texfile (file-readable-p texfile))
	  (insert-file-contents texfile)
	(insert-buffer-substring cb))
      (save-match-data
	(goto-char (point-min))
	(cond
	 ;; bibtex
	 ((re-search-forward "\\\\\\(?:no\\)*bibliography{\\(.*?\\)}" nil t)
	  (mapcar (lambda (fname)
		    (if (file-name-extension fname)
			(format "%s%s" dir fname)
		      (format "%s%s.bib" dir fname)))
		  (split-string (match-string-no-properties 1) ",[ ]*")))
	 ;; biblatex
	 ((re-search-forward "\\\\addbibresource\\(\\[.*?\\]\\)?{\\(.*?\\)}" nil t)
	  (mapcar (lambda (fname)
		    (if (file-name-extension fname)
			(format "%s%s" dir fname)
		      (format "%s%s.bib" dir fname)))
		  (let ((option (match-string 1))
			(file (match-string-no-properties 2)))
		    (unless (and option (string-match-p "location=remote" option))
		      (split-string file ",[ ]*")))))
	 (t
	  bibtex-completion-bibliography))))))


;; ==================================================================
;; ˚˚ labels and cross-referencing
;; ==================================================================

;;;###autoload
(defun power-ref-utils ()
  (interactive)
  (let ((labels (org-ref-get-labels)))
    (helm :sources `(,(helm-build-sync-source "Existing labels"
			:candidates labels
			:action `(("Insert ref link" .
				   (lambda (label)
				     (let* ((object (org-element-context))
					    (last-char
					     (save-excursion
					       (goto-char (org-element-property :end object))
					       (backward-char)
					       (if (looking-at " ")
						   " "
						 ""))))
				       (if (-contains? '("ref" "eqref" "pageref" "nameref")
						       (org-element-property :type object))
					   ;; we are on a link, so replace it.
					   (setf
					    (buffer-substring
					     (org-element-property :begin object)
					     (org-element-property :end object))
					    (concat
					     (replace-regexp-in-string
					      (org-element-property :path object)
					      label
					      (org-element-property :raw-link object))
					     last-char))
					 (insert
					  (concat
					   " ref:" label))))))
				  ("Insert alternate link" .
				   (lambda (label)
				     (insert
				      (helm :sources (helm-build-sync-source "Ref link types"
						       :candidates '("ref" "vref" "pageref"
								     "nameref" "eqref")
						       :action (lambda (x)
								 ;; here we try to emulate vref behaviour
								 (if (string= x "vref")
								     (format " ref:%s on page pageref:%s"
									     label label)
								   (format " %s:%s" x label))))))))
				  ("Insert custom-id link" .
				   (lambda (label)
				     (insert
				      (format "[[#%s]]" label))))))
		     ,(helm-build-dummy-source "Create new label"
			:action (lambda (label)
				  (with-helm-current-buffer
				    (insert (concat "label:" label)))))
		     ,(helm-build-sync-source "Utilities"
			:candidates '(("Insert glossary" . org-ref-insert-glossary-link)
				      ("Insert figure" . power-ref-insert-figure)
				      ;; FIXME: latex fragments in caption
				      ("List of figures" . org-ref-list-of-figures)
				      ("List of tables" . org-ref-list-of-tables))
			:action (lambda (x)
                                  (funcall x))))
	  :buffer "*helm utils*")))


;; ==================================================================
;; ˚˚ utils
;; ==================================================================

;;;###autoload
(defun power-ref-bad-keys ()
  "Check for bad keys in both pdf directory and notes file."
  (interactive)
  (let ((keys '())
	(pkeys (mapcar (lambda (x)
			 (replace-regexp-in-string "\\.pdf$" "" x))
		       (directory-files org-ref-pdf-directory nil "\\.pdf$")))
	(bibkeys (bibtex-completion-candidates)))
    (setq allkeys (mapcar (lambda (x)
			    (cdr (assoc "=key=" x)))
			  bibkeys))
    (setq badkeys (set-difference pkeys allkeys :test 'equal))
    (with-temp-buffer
      (insert-file-contents org-ref-bibliography-notes)
      (save-restriction
	(goto-char (point-min))
	(while (re-search-forward "^[ \t]*:Custom_ID: \\(.+\\)$" nil t)
	  (when (thing-at-point 'symbol)
	    (let ((key (thing-at-point 'symbol t)))
	      (when (not (-contains? allkeys key))
		(push (cons key (point)) keys))))))
      (helm :sources `(,(helm-build-sync-source "Bad pdf keys"
			  :candidates (if badkeys badkeys '("No bad keys"))
			  :action (lambda (candidate)
				    (dired
				     (concat org-ref-pdf-directory "/" candidate "\.pdf"))))
		       ,(helm-build-sync-source "Bad note keys"
			  :candidates (lambda ()
					(if keys keys '("No bad keys")))
			  :action (lambda (marker)
				    (switch-to-buffer
				     (find-file-noselect org-ref-bibliography-notes))
				    (goto-char marker)
				    (org-show-entry))))))))

;;;###autoload
(defun power-ref-bad-citations ()
  "Show bad citations, ref links and labels."
  (interactive)
  (let ((cb (current-buffer))
        (bad-citations (org-ref-bad-cite-candidates))
        (bad-refs (org-ref-bad-ref-candidates))
        (bad-labels (org-ref-bad-label-candidates))
        (bad-files (org-ref-bad-file-link-candidates))
        (bib-candidates '()))
    (cl-pushnew
     (cons (format  "Using these bibtex files: %s"
                    (org-ref-find-bibliography))
           (lambda () nil))
     bib-candidates)

    ;; Check bibliography style exists
    (save-excursion
      (goto-char 0)
      (unless (re-search-forward "bibliographystyle:\\|\\\\bibliographystyle{" nil t)
        (cl-pushnew
	 (cons "No bibliographystyle found."
	       (lambda ()
		 (switch-to-buffer "*org-ref*")
		 (erase-buffer)
		 (insert "No bibliography style found. This may be ok, if your latex class style sets that up, but if not this is an error. Try adding something like:
bibliographystyle:unsrt
at the end of you file.
")
		 (org-mode)))
	 bib-candidates)))

    ;; Check if latex knows of the bibliographystyle. We only check links here.
    ;;  I also assume this style exists as a bst file that kpsewhich can find.
    (save-excursion
      (goto-char 0)
      (when (re-search-forward "bibliographystyle:" nil t)
        ;; on a link. get style
        (let ((path (org-element-property :path (org-element-context))))
          (unless (= 0 (shell-command (format "kpsewhich %s.bst" path)))
            (cl-pushnew
	     (cons (format "bibliographystyle \"%s\" may be unknown" path)
		   (lambda ()
		     (goto-char 0)
		     (re-search-forward "bibliographystyle:")))
	     bib-candidates)))))

    ;; check for multiple bibliography links
    (let* ((bib-links (-filter
                       (lambda (el)
                         (string= (org-element-property :type el) "bibliography"))
                       (org-element-map (org-element-parse-buffer) 'link 'identity)))
           (n-bib-links (length bib-links)))

      (when (> n-bib-links 1)
        (mapc (lambda (link)
                (setq
                 bib-candidates
                 (append
                  bib-candidates
                  (list (cons (format  "Multiple bibliography link: %s"
				       (org-element-property :raw-link link))
                              `(lambda ()
                                 (goto-char ,(org-element-property :begin link))))))))
              bib-links)))

    ;; Check for bibliography files existence.
    (mapc (lambda (bibfile)
            (unless (file-exists-p bibfile)
              (cl-pushnew
	       (cons
		(format "%s does not exist." bibfile)
		(lambda ()
		  (message "Non-existent bibfile.")))
	       bib-candidates)))
          (org-ref-find-bibliography))

    ;; check for spaces in bibliography
    (let ((bibfiles (mapcar 'expand-file-name
                            (org-ref-find-bibliography))))
      (mapc (lambda (bibfile)
              (when (string-match " " bibfile)
                (cl-pushnew
                 (cons (format "One or more spaces found in path to %s" bibfile)
                       (lambda ()
                         (message "No spaces are allowed in bibtex file paths. We recommend replacing them with -. Underscores usually cause other problems if you don't know what you are doing.")))
		 bib-candidates)))
            bibfiles))

    ;; validate bibtex files
    (let ((bibfiles (mapcar 'expand-file-name
                            (org-ref-find-bibliography))))
      (mapc
       (lambda (bibfile)
         (unless (with-current-buffer
                     (find-file-noselect bibfile)
                   (bibtex-validate))
           (cl-pushnew
    	    (cons
    	     (format  "Invalid bibtex file found. %S" bibfile)
    	     `(lambda ()
    		(find-file ,bibfile)))
    	    bib-candidates)))
       bibfiles))
    (helm :sources `(((name . "Bad citations")
                      (candidates . ,bad-citations)
                      (action . (lambda (marker)
                                  (switch-to-buffer (marker-buffer marker))
                                  (goto-char marker)
				  (org-show-entry))))
                     ;;
                     ((name . "Multiple labels")
                      (candidates . ,bad-labels)
                      (action . (lambda (marker)
                                  (switch-to-buffer (marker-buffer marker))
                                  (goto-char marker)
				  (org-show-entry))))
                     ;;
                     ((name . "Bad ref links")
                      (candidates . ,bad-refs)
                      (action . (lambda (marker)
                                  (switch-to-buffer (marker-buffer marker))
                                  (goto-char marker)
				  (org-show-entry))))
                     ;;
                     ((name . "Bad file links")
                      (candidates . ,bad-files)
                      (lambda (marker)
                        (switch-to-buffer (marker-buffer marker))
                        (goto-char marker)
			(org-show-entry)))

                     ((name . "Bibliography")
                      (candidates . ,bib-candidates)
                      (action . (lambda (x)
                                  (switch-to-buffer ,cb)
                                  (funcall x))))))))


;; ==================================================================
;; ˚˚ keymap
;; ==================================================================

(defvar power-ref-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    ;; mnemonics
    (define-key map (kbd "C-M-p") 'power-ref-open-pdf)
    (define-key map (kbd "C-M-c") 'power-ref-insert-citation)
    (define-key map (kbd "C-M-n") 'power-ref-edit-notes)
    (define-key map (kbd "C-M-k") 'power-ref-tag-entries)
    (define-key map (kbd "C-M-e") 'power-ref-show-entry)
    (define-key map (kbd "C-M-a") 'power-ref-prepare-annotation)
    map))

(defun power-ref-process-key ()
  "Remove counter from key."
  (replace-regexp-in-string "^[0-9]+? " "" (car (helm-marked-candidates))))

(defun power-ref-open-pdf (arg)
  (interactive "P")
  (power-ref-process-key)
  (if arg
      (let* ((key (car (helm-marked-candidates)))
	     (pdf (car (bibtex-completion-find-pdf-in-library key))))
	(when pdf
	  (helm-exit-and-execute-action (browse-url pdf))))
    (helm-exit-and-execute-action 'helm-bibtex-open-pdf)))

(defun power-ref-insert-citation (arg)
  "Insert citation at point.
With a prefix ARG, prompt for pre and postnotes. See
`power-ref-number-of-optional-arguments' for more information."
  (interactive "P")
  (if arg
      (setq power-ref-number-of-optional-arguments 2)
    (setq power-ref-number-of-optional-arguments
	  power-ref-preferred-number-of-optional-arguments))
  (helm-exit-and-execute-action 'helm-bibtex-insert-citation))

(defun power-ref-edit-notes ()
  (interactive)
  (helm-exit-and-execute-action
   (lambda (key)
     (let* ((cand (car (helm-marked-candidates)))
     	    (key (replace-regexp-in-string "^[0-9]+? " "" cand)))
       (bibtex-completion-edit-notes key)))))

(defun power-ref-tag-entries ()
  (interactive)
  (helm-exit-and-execute-action 'org-ref-helm-tag-entries))

(defun power-ref-show-entry ()
  (interactive)
  (power-ref-process-key)
  (helm-exit-and-execute-action
   (lambda (key)
     (let* ((cand (car (helm-marked-candidates)))
	    (key (replace-regexp-in-string "^[0-9]+? " "" cand)))
       (bibtex-completion-show-entry key)))))

(defun power-ref-prepare-annotation ()
  (interactive)
  (power-ref-process-key)
  (helm-exit-and-execute-action 'power-ref-annotate))

;; ==================================================================
;; ˚˚ browser
;; ==================================================================

;; propertize candidates

(defun power-ref-propertize (candidates)
  (cl-loop for i in candidates
	   collect (concat (propertize i 'font-lock-face `(:foreground ,org-ref-cite-color)))))

;; browse labels

(defun org-ref-browse-labels ()
  "Browse existing labels in the current buffer."
  (let ((labels (org-ref-get-labels)))
    (helm :sources `(,(helm-build-sync-source "Browse labels"
			:follow 1
			:candidates labels
			:action (lambda (label)
				  (with-helm-current-buffer
				    (org-open-link-from-string
				     (format "ref:%s" label)))))
			:persistent-action (lambda (label)
					     (with-helm-current-buffer
					       (org-open-link-from-string
						(format "ref:%s" label)))
					     (helm-highlight-current-line nil nil nil nil 'pulse)))
	  :buffer "*helm labels*")))

;; browse citation links

(defun org-ref-browser-open-menu (candidate)
  (goto-char
   (cdr (assoc candidate alist1)))
  (org-open-at-point))

(defun org-ref-browser-transformer (candidates)
  "Add counter to candidates."
  (let ((counter 0))
    (cl-loop for i in candidates
	     collect (format "%s %s" (cl-incf counter) i))))

(defun org-ref-browser-display (candidate)
  "Strip counter from candidates."
  (replace-regexp-in-string "^[0-9]+? " "" candidate))

;;;###autoload
(defun org-ref-browser-goto-citation-links (&optional bibkey)
  "Quickly browse citation links in the current buffer.
With an optional BIBKEY argument, narrow to citation links that share
the same key."
  (interactive)
  (let ((keys nil)
	(alist nil))
    (widen)
    (show-all)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
	(let ((plist (nth 1 link)))
	  (when (-contains? org-ref-cite-types (plist-get plist ':type))
	    (let ((start (org-element-property :begin link)))
	      (dolist (key
		       (org-ref-split-and-strip-string (plist-get plist ':path)))
		;; when called from org-ref-browser, narrow to
		;; citation links matching the current candidate.
		;; Otherwise use all keys as candidates.
		(if bibkey
		    (when (string= key bibkey)
		      (setq keys (append keys (list key)))
		      (setq alist (append alist (list (cons key start)))))
		  (setq keys (append keys (list key)))
		  (setq alist (append alist (list (cons key start)))))))))))
    (let ((counter 0))
      ;; the idea here is to create an alist with ("counter key" .
      ;; position) to produce unique candidates
      (setq count-key-pos (mapcar (lambda (x)
				    (cons
				     (format "%s %s" (cl-incf counter) (car x)) (cdr x)))
				  alist)))
    ;; push mark to restore position with C-u C-SPC
    (push-mark (point))
    ;; move point to the first citation link in the buffer
    (goto-char (cdr (assoc (caar alist) alist)))
    (helm :sources
	  (helm-build-sync-source "Browse citation links"
	    :follow 1
	    :candidates keys
	    :candidate-transformer 'org-ref-browser-transformer
	    :real-to-display 'org-ref-browser-display
	    :persistent-action (lambda (candidate)
				 (helm-goto-char
				  (cdr (assoc candidate count-key-pos)))
				 (helm-highlight-current-line nil nil nil nil 'pulse))
	    :action `(("Open menu" . ,(lambda (candidate)
					(helm-goto-char
					 (cdr (assoc candidate count-key-pos)))
					(org-open-at-point)))))
	  :buffer "*helm goto links*")))

;; ==================================================================
;; ˚˚ sources
;; ==================================================================

(defvar power-ref-bibtex-source
  (helm-build-sync-source "BibTeX entries"
    :init 'bibtex-completion-init
    :candidates 'bibtex-completion-candidates
    :filtered-candidate-transformer 'helm-bibtex-candidates-formatter
    :action power-ref-actions
    ;; for keybindings to work after helm-resume, we need the keymap
    ;; set to helm source instead of helm session
    :keymap power-ref-map))

(defvar power-ref-fallback-source
  (helm-build-sync-source "Fallback options"
    :candidates 'bibtex-completion-fallback-candidates
    :match (lambda (_candidate) t)
    :action 'bibtex-completion-fallback-action))

;;;###autoload
(defun power-ref (&optional arg)
  "Return a list of references in a helm buffer.
With one prefix ARG, open utility functions. With two prefix
arguments, validate the bibtex file and check for bad links.

With three prefix arguments the cache is invalidated and the
bibliography reread."
  (interactive "P")
  (let ((bibtex-completion-bibliography
	 (if (or (eq major-mode 'latex-mode)
		 (eq major-mode 'org-mode))
	     (bibtex-completion--get-local-databases)
	   bibtex-completion-bibliography)))
    (when (equal arg '(64))
      (setq bibtex-completion-bibliography-hash ""))
    (cond
     ((equal arg '(4))
      (power-ref-utils))
     ((equal arg '(16))
      (power-ref-bad-citations))
     ((or (equal arg nil)
	  (equal arg '(64)))
      (helm :sources '(power-ref-bibtex-source power-ref-fallback-source)
	    :buffer "*power ref*"
	    :candidate-number-limit 1000)))))

(provide 'citation-utils)
;;; citation-utils.el ends here
