;;; citation-utils.el --- Citation utilities for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Code here is experimental.

;;; Code:

(require 'org-ref)
(require 'helm-bibtex)

(defcustom power-ref-actions
  '(("Open PDF              C-M-p"   . helm-bibtex-open-pdf)
    ("Insert citation       C-M-c"   . helm-bibtex-insert-citation)
    ("Edit notes            C-M-n"   . bibtex-completion-edit-notes)
    ("Add keywords          C-M-k"   . power-ref-tag-entries)
    ("Insert notes template C-M-i\n" . power-ref-insert-notes-template)
    ("Show entry"                    . bibtex-completion-show-entry)
    ("Open URL or DOI"               . helm-bibtex-open-url-or-doi)
    ("Insert reference"              . helm-bibtex-insert-reference)
    ("Copy key"                      . power-ref-copy-key)
    ("Attach PDF to email"           . helm-bibtex-add-PDF-attachment))
  "Cons cells of string and function to set the actions of `helm-bibtex' to.
The car of cons cell is the string describing the function. The cdr of
the the cons cell is the function to use."
  :type '(alist :key-type string :value-type function))

(defcustom power-ref-number-of-optional-arguments 2
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
   (cons (s-format "$0 $1 $2 $3 $4 $5 $6" 'elt
                   (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s))
                              fields (list 25 (- width 65) 4 1 1 7 14)))
         entry-key)))

(defun power-ref-insert-notes-template (_)
  "Insert notes template associated with key."
  (let* ((keys (helm-marked-candidates :with-wildcard t))
	 (entry (bibtex-completion-get-entry (car keys))))
    (with-helm-current-buffer
      (insert (s-format bibtex-completion-notes-template-one-file
			'bibtex-completion-apa-get-value
			entry)))))

(defun power-ref-copy-key (candidates)
  "Add bibtex key to the kill-ring."
  (let* ((key (helm-marked-candidates)))
    (kill-new (car key))))

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

;; propertize candidates
 
(defun power-ref-propertize (candidates)
  (cl-loop for i in candidates
	   collect (concat (propertize i 'font-lock-face `(:foreground ,org-ref-cite-color)))))

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
                     ((name . "Multiply defined labels")
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

;;;###autoload
(defun power-ref-utils ()
  "A few useful functions from org-ref."
  (interactive)
  (let* ((labels (org-ref-get-labels))
         (cb (current-buffer)))
    (helm :sources `(((name . "Existing labels")
                      (candidates . ,labels)
                      ;; default action is to open to the label
                      (action . (lambda (label)
                                  ;; unfortunately I do not have markers here
                                  (org-open-link-from-string
                                   (format "ref:%s" label))))
                      ;; if you select a label, replace current one
                      (action . (lambda (label)
                                  (switch-to-buffer ,cb)
                                  (cond
                                   ;;  no prefix or on a link
                                   ((equal helm-current-prefix-arg nil)
                                    (let* ((object (org-element-context))
                                           (last-char
					    (save-excursion
					      (goto-char (org-element-property :end object))
					      (backward-char)
					      (if (looking-at " ")
						  " "
						""))))
                                      (when (-contains?
					     '("label")
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
                                          last-char)))))))))
                     ;; no matching selection creates a new label
                     ((name . "Create new label")
                      (dummy)
                      ;; default action creates a new label, or replaces old one
                      (action . (lambda (label)
                                  (switch-to-buffer ,cb)
                                  (let* ((object (org-element-context))
                                         (last-char
					  (save-excursion
					    (goto-char (org-element-property :end object))
					    (backward-char)
					    (if (looking-at " ")
						" "
					      ""))))
                                    (if (-contains? '("label")
                                                    (org-element-property :type object))
                                        ;; we are on a link, so replace it.
                                        (setf
                                         (buffer-substring
                                          (org-element-property :begin object)
                                          (org-element-property :end object))
                                         (concat
                                          (replace-regexp-in-string
                                           (org-element-property :path object)
                                           helm-pattern
                                           (org-element-property :raw-link object))
                                          last-char))
                                      ;; new link
                                      (insert
                                       (concat
                                        "label:"
                                        (or label
                                            helm-pattern))))))))
		     ((name . "Utilities")
		      (candidates . (("Insert ref link" . org-ref-helm-insert-ref-link)
				     ("Insert figure" . power-ref-insert-figure)
				     ("List of figures" . org-ref-list-of-figures)
				     ("List of tables" . org-ref-list-of-tables)))
		      (action . (lambda (x)
                                  (switch-to-buffer ,cb)
                                  (funcall x)))))
	  :buffer "*power ref utils*")))

;; keymap

(defvar power-ref-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    ;; mnemonics
    (define-key map (kbd "C-M-p") 'power-ref-open-pdf)
    (define-key map (kbd "C-M-c") 'power-ref-insert-citation)
    (define-key map (kbd "C-M-n") 'power-ref-edit-notes)
    (define-key map (kbd "C-M-k") 'power-ref-tag-entries)
    map))

(defun power-ref-open-pdf ()
  (interactive)
  (helm-exit-and-execute-action 'helm-bibtex-open-pdf))

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
  (helm-exit-and-execute-action 'bibtex-completion-edit-notes))

(defun power-ref-tag-entries ()
  (interactive)
  (helm-exit-and-execute-action 'org-ref-helm-tag-entries))

;;;###autoload
(defun power-ref (&optional arg)
  "Return a list of citation links and references in a helm buffer.
With one prefix ARG, open utility functions. With two prefix
arguments, validate the bibtex file and check for bad links."
  (interactive "P")
  (cond
   ((equal arg '(4))
    (power-ref-utils))
   ((equal arg '(16))
    (power-ref-bad-citations))
   ((equal arg nil)
      (let ((keys '()))
	(org-element-map (org-element-parse-buffer) 'link
	  (lambda (link)
	    (let ((plist (nth 1 link)))
	      (when (-contains? org-ref-cite-types (plist-get plist ':type))
		(dolist (key
			 (org-ref-split-and-strip-string (plist-get plist ':path)))
		  (when (not (-contains? keys key))
		    (setq keys (append keys (list key)))))))))
	(helm :sources `(((name . "Citation links")
			  (candidates . ,(mapcar (lambda (x)
						   (format "%s" x))
						 (nreverse keys))) ; TODO sort by most cited
			  (candidate-transformer power-ref-propertize)
			  ;; TODO use org-ref-cite-candidates instead
			  (action . ,power-ref-actions))
			 ((name . "BibTeX entries")
			  (init . bibtex-completion-init)
			  (candidates . bibtex-completion-candidates)
			  (filtered-candidate-transformer . helm-bibtex-candidates-formatter)
			  (action . ,power-ref-actions))
			 ((name . "Fallback options")
			  (match (lambda (_candidate) t))
			  (candidates . bibtex-completion-fallback-candidates)
			  (action . bibtex-completion-fallback-action)))
	      :buffer "*power ref*"
	      :keymap power-ref-map
	      :candidate-number-limit 1000)))))

(provide 'citation-utils)
;;; citation-utils.el ends here
