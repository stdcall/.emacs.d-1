;;; config-bibtex.el --- bibtex-mode configuration file

;; generate key automatically by pressing C-c C-c

(setq bibtex-autokey-name-year-separator ""
      bibtex-autokey-name-case-convert 'identity
      bibtex-autokey-year-title-separator ""
      bibtex-autokey-year-length 4
      bibtex-autokey-titleword-separator ""
      bibtex-autokey-titlewords 1
      bibtex-autokey-titleword-length 1
      bibtex-autokey-titlewords-stretch 0
      bibtex-autokey-titleword-first-ignore '("the" "a" "o" "on" "an" "in"))

;; check if the key is unique
;; https://github.com/gwierink/dotemacsdotd/blob/master/init-latex.el

(defun my-bibtex-autokey-unique (key)	;TODO: use letters instead
  "Make a unique version of KEY
   by converting the last character to an integer 1-9."
  (save-excursion
    (let ((trykey key)
          (next ?1))
      (while (and (bibtex-find-entry trykey t)
                  (<= next ?9))
        (aset trykey (1- (length trykey)) next)
        (setq next (1+ next)))
      trykey)))

(setq bibtex-autokey-before-presentation-function 'my-bibtex-autokey-unique)

;; press C-c C-q to format individual entries, or M-x
;; bibtex-reformat to format all entries in the buffer

(setq bibtex-align-at-equal-sign t)
(setq bibtex-text-indentation 14)
(setq bibtex-field-indentation 2)
(setq bibtex-contline-indentation 11)
(setq bibtex-entry-format
      '(opts-or-alts
	required-fields
	numerical-fields
	page-dashes
	whitespace
	realign
	))

;; the output is as follows:

;; @article{Becker1996a,
;;   author    = {Becker, Howard},
;;   journal   = {Mana},
;;   number    = {2},
;;   pages     = {177-188},
;;   title     = {A Escola de Chicago},
;;   volume    = {2},
;;   year      = {1996}
;; }

;;; keep file updated everytime it is saved

(add-hook 'bibtex-mode-hook 
          (lambda () 
	    (add-hook 'after-save-hook #'jag/last-update nil 'make-it-local)))

(defun jag/last-update ()
  "Append timestamp and total number of entries. See
`time-stamp-format' for possible string replacements."
  (interactive)
  (save-excursion			;preserve point position
    (let ((time-stamp-format "%%%% %f. Last modified on %:y-%02m-%02d %02H:%02M,"))
      (beginning-of-buffer)
      (kill-line)
      (insert (time-stamp-string))
      (jag/bibtex-count-entries)
      (set-buffer-modified-p nil)	;FIXME:
      )))

(defun jag/bibtex-count-entries (&optional count-string-entries)
  "Insert the number of entries in the current buffer at point.
See `bibtex-count-entries' for a more thorough explanation of the
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

;; M-x bibtex-validate to validate all entries in the buffer
;; TODO: align but don't fill
