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

(defcustom bib-config (when load-file-name
			(concat (file-name-directory load-file-name) "config-bibtex.el"))
  "The path to the bibtex configuration file.")

;; generate key automatically

(setq bibtex-autokey-name-year-separator ""
      bibtex-autokey-name-case-convert 'identity
      bibtex-autokey-year-title-separator ""
      bibtex-autokey-year-length 4
      bibtex-autokey-titleword-separator ""
      bibtex-autokey-titlewords 1
      bibtex-autokey-titleword-length 1
      bibtex-autokey-titlewords-stretch 0
      bibtex-autokey-titleword-ignore nil)

;; cleanup entries

(setq bibtex-autokey-before-presentation-function #'bibtex-capitalize-key)

(defun bibtex-capitalize-key (key)
  "Capitalize BibTeX key before generated key is presented. See
`bibtex-autokey-before-presentation-function'."
  (save-excursion
    (bibtex-search-entry key)
    (capitalize key))
  (message "%s" key))

(defun bibtex-delete-key ()
  "Delete BibTeX key of the entry at point."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
	(delete-region (match-beginning bibtex-key-in-head)
		       (match-end bibtex-key-in-head)))))

(defun bibtex-cleanup-entry ()
  "Delete old key before generating a new one and cleanup entry.
See `bibtex-clean-entry', `bibtex-replace-nonascii' and
`bibtex-replace-naked-ampersand' for more information."
  (interactive)
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (insert-file-contents bib-config)
      (eval-buffer)
      (switch-to-buffer buf))
    (bibtex-delete-key)
    (bibtex-replace-nonascii)
    (bibtex-replace-naked-ampersand)
    (bibtex-clean-entry)))

(add-hook 'bibtex-mode-hook
     (lambda ()
      (define-key bibtex-mode-map (kbd "C-c C-c") #'bibtex-cleanup-entry)))

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

;; press C-c C-q to format individual entries, or M-x bibtex-reformat
;; to format all entries in the buffer

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
  "Append timestamp and total number of entries. See
`time-stamp-format' for possible string replacements."
  (save-excursion
    (let ((time-stamp-format "%%%% %f. Last modified on %:y-%02m-%02d %02H:%02M,"))
      (beginning-of-buffer)
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

(provide 'config-bibtex)
;;; config-bibtex.el ends here
