;;; config-bibtex.el --- bibtex-mode configuration file

;;; Tips:
;; - enclose words in double braces to ensure that capitalization is maintained
;; - check nonstandard author names, e.g. Paulo {dos Santos}
;; - separate names of authors with "and", not commas

;;; Useful commands:
;; C-c C-b to insert new (b)ibtex entry
;; C-j to (j)ump to the next field
;; C-c C-f to insert new (f)ield
;; C-c C-d to (d)elete field content
;; C-c C-k to (k)ill field
;; C-c C-c to (c)leanup entry
;; M-x bibtex-validade to check for syntactic mistakes
;; M-x bibtex-reformat to reformat entries

;;; See also:
;; https://github.com/jkitchin/org-ref/blob/master/jmax-bibtex.el
;; http://www.brl.ntt.co.jp/people/leroux/bibtex-mode.html

;;; generate key automatically
;; ie. Smith2015s (author's last name / year / first letter of the
;; title). Increasing the value of bibtex-autokey-titleword-length
;; should increase the key's entropy and avoid yielding duplicate
;; keys.

(setq bibtex-autokey-name-year-separator ""
      bibtex-autokey-year-title-separator ""
      bibtex-autokey-year-length 4
      bibtex-autokey-titleword-separator ""
      bibtex-autokey-titlewords 1
      bibtex-autokey-titleword-length 1
      bibtex-autokey-titlewords-stretch 0
      bibtex-autokey-titleword-ignore nil)

;; cleanup entries

(defun bibtex-cleanup-entry ()
  "Cleanup bibtex entry."
  (interactive)
  (bibtex-beginning-of-entry)
  (save-excursion
    (bibtex-delete-key)
    (bibtex-replace-nonascii)
    (bibtex-replace-naked-ampersand)
    (bibtex-last-comma)
    (bibtex-clean-entry)
    (let ((key (bibtex-key-in-head)))
      (when (bibtex-key-in-head)
	(message "Formatting %s (done)" key)))))

(add-hook 'bibtex-mode-hook
     (lambda ()
      (define-key bibtex-mode-map (kbd "C-c C-c") #'bibtex-cleanup-entry)))

(setq bibtex-autokey-before-presentation-function 'bibtex-capitalize-key)

(defun bibtex-capitalize-key (key)
  "Capitalize bibtex key before generated key is presented. See
`bibtex-autokey-before-presentation-function'."
  (save-excursion
    (let ((cap-key (capitalize key)))
      (bibtex-search-entry cap-key)
      (message "%s" cap-key))))

(defun bibtex-delete-key ()
  "Delete bibtex key of the entry at point."
  (save-excursion
    (bibtex-beginning-of-entry)
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
	(delete-region (match-beginning bibtex-key-in-head)
		       (match-end bibtex-key-in-head)))))

;; replace non-ascii characters

(defvar bibtex-nonascii-latex-replacements '()
  "Cons list of common Portuguese non-ascii characters and their
  LaTeX representations.")

(setq bibtex-nonascii-latex-replacements
      '(("á" . "{\\\\'a}")
	("ã" . "{\\\\~a}")
	("â" . "{\\\\^a}")
	("à" . "{\\\\`a}")
	("é" . "{\\\\'e}")
	("ê" . "{\\\\^e}")
	("í" . "{\\\\'i}")
	("ó" . "{\\\\'o}")
	("õ" . "{\\\\~o}")
	("ô" . "{\\\\^o}")
	("ú" . "{\\\\'u}")
	("ç" . "{\\\\c c}")
	("‘" . "'")
	("’" . "'")
	("“" . "\"")
	("”" . "\"")))

(defun bibtex-replace-nonascii ()
  "Replace non-ascii characters in a bibtex entry."
  (interactive)
  (save-restriction
    (bibtex-narrow-to-entry)
    (goto-char (point-min))
    (dolist (char (mapcar (lambda (x) (car x)) bibtex-nonascii-latex-replacements))
      (while (re-search-forward char nil t)
	(replace-match (cdr (assoc char bibtex-nonascii-latex-replacements))))
      (goto-char (point-min)))))

(defun bibtex-replace-naked-ampersand ()
  "Replace naked ampersand with its corresponding LaTeX
equivalent."
  (interactive)
  (save-restriction
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (replace-regexp " & " " \\\\& ")
    (widen)))

(defun bibtex-last-comma ()
  "Insert or delete comma after last field according to
`bibtex-comma-after-last-field'."
  ;; the variable had no effect on the original function. I think
  ;; because looking-at was used instead of looking-back.
  (interactive)
  (save-excursion
    (bibtex-end-of-entry)
    (previous-line)
    (end-of-line)
    (if (and bibtex-comma-after-last-field (not (looking-back ",")))
	(insert ","))
    (if (and (not bibtex-comma-after-last-field)
	     (looking-back ","))
	(delete-backward-char 1))))

;; press C-c C-q to reformat individual entries, or M-x
;; bibtex-reformat to reformat all entries

(setq bibtex-align-at-equal-sign t)
(setq bibtex-entry-offset 0)
(setq bibtex-text-indentation 14)
(setq bibtex-field-indentation 2)
(setq bibtex-contline-indentation 20)
(setq bibtex-autokey-edit-before-use nil)
(setq bibtex-entry-format
      '(opts-or-alts
	required-fields
	numerical-fields
	page-dashes
	whitespace
	realign))

;; align entries but don't fill

(advice-add 'bibtex-fill-field-bounds :around #'jag/bibtex-fill-field-bounds)

(defun jag/bibtex-fill-field-bounds (bibtex-fill-field-bounds bounds justify &optional move)
  "Format BibTeX field delimited by BOUNDS.
If JUSTIFY is non-nil justify as well. If optional arg MOVE is
non-nil move point to end of field."
  (let ((end-field (copy-marker (bibtex-end-of-field bounds))))
    (if (not justify)
        (goto-char (bibtex-start-of-text-in-field bounds))
      (goto-char (bibtex-start-of-field bounds))
      (forward-char) ; leading comma
      (bibtex-delete-whitespace)
      (insert "\n")
      (indent-to-column (+ bibtex-entry-offset
                           bibtex-field-indentation))
      (re-search-forward "[ \t\n]*=" end-field)
      (replace-match "=")
      (forward-char -1)
      (if bibtex-align-at-equal-sign
          (indent-to-column
           (+ bibtex-entry-offset (- bibtex-text-indentation 2)))
        (insert " "))
      (forward-char)
      (bibtex-delete-whitespace)
      (if bibtex-align-at-equal-sign
          (insert " ")
        (indent-to-column bibtex-text-indentation)))
    (if move (goto-char end-field))))

;; keep file updated everytime it is saved

(add-hook 'bibtex-mode-hook
          (lambda ()
	    (add-hook 'after-save-hook #'bibtex-last-update nil 'make-it-local)))

(defun bibtex-last-update ()
  "Append timestamp and total number of entries as a comment on
the first line of the file. See `time-stamp-format' for possible
string replacements."
  (save-excursion
    (let ((time-stamp-format "%%%% %f. Last modified on %:y-%02m-%02d %02H:%02M,"))
      (goto-char (point-min))
      (delete-region
       (point)
       (save-excursion
	 (move-end-of-line 1) (point)))
      (insert (time-stamp-string))
      (jag/bibtex-count-entries)
      (set-buffer-modified-p nil))))

(defun jag/bibtex-count-entries (&optional count-string-entries)
  "Insert the total number of entries in the current buffer. See
`bibtex-count-entries' for a more thorough explanation of the
original function."
  (interactive)
  (let ((number 0)
        (bibtex-sort-ignore-string-entries (not count-string-entries)))
    (save-restriction
      (if mark-active (narrow-to-region (region-beginning) (region-end)))
      (bibtex-map-entries (lambda (_key _beg _end) (setq number (1+ number)))))
    (insert (format " with %d entries." number))))

(defun bibtex-set-field (field value &optional nodelim)
  "Set FIELD to VALUE in bibtex file. Create field if it does not exist.
Optional argument NODELIM see `bibtex-make-field'."
  (interactive "sField: \nsValue: ")
  (bibtex-beginning-of-entry)
  (let ((found))
    (if (setq found (bibtex-search-forward-field field t))
	;; we found a field
	(progn
	  (goto-char (car (cdr found)))
	  (when value
	    (bibtex-kill-field)
	    (bibtex-make-field field nil nil nodelim)
	    (backward-char)
	    (insert value)))
      ;; make a new field
      (message "new field being made")
      (bibtex-beginning-of-entry)
      (forward-line) (beginning-of-line)
      (bibtex-next-field nil)
      (forward-char)
      (bibtex-make-field field nil nil nodelim)
      (backward-char)
      (insert value))))

(defun bibtex-get-doi ()
  "Search doi online."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let ((author (bibtex-autokey-get-field "author"))
	  (title (bibtex-autokey-get-field "title")))
      	(browse-url
	 (format "http://search.crossref.org/?q=%s %s" author title)))))

(provide 'config-bibtex)
;;; config-bibtex.el ends here
