;;; config-search.el --- search online

(key-chord-define-global
 "qq"
 (defhydra hydra-query (:color teal :columns 1)
   "
"
   ("j" search-duckduckgo-or-browse-url "duckduckgo")
   ("k" search-github "github")
   ("l" search-gist "gist")
   (";" search-google-scholar "scholar")
   ("q" nil "quit")))

(defun search-duckduckgo-or-browse-url (&optional arg)
  "Search selected region online.
If `thing-at-point' matches url, open it with default browser. With a
prefix ARG, enclose region in quotes. If nothing is selected, prompt
for a string in the minibuffer."
  (interactive "P")
  (let ((str (url-hexify-string (buffer-substring-no-properties
				 (region-beginning)
				 (region-end))))
	(url (thing-at-point 'url)))
    (if url
	(browse-url url)
      (browse-url
       (format
	"https://duckduckgo.com/?q=%s"
	(if (use-region-p)
	    (if arg
		(concat "\"" (url-unhex-string str) "\"")
	      str)
	  (read-from-minibuffer "DuckDuckGo: ")))))))

(defun search-github ()
  "Search github using emacs-lisp as the default language."
  (interactive)
  (browse-url
   (format
    "https://github.com/search?l=emacs-lisp&q=%s&type=Code"
    (if (use-region-p)
	(url-hexify-string (buffer-substring
			    (region-beginning)
			    (region-end)))
      (read-from-minibuffer "GitHub: ")))))

(defun search-gist ()
  "Search gist using emacs-lisp as the default language."
  (interactive)
  (browse-url
   (format
    "https://gist.github.com/search?q=language:emacs-lisp+%s"
    (if (use-region-p)
	(url-hexify-string (buffer-substring
			    (region-beginning)
			    (region-end)))
      (read-from-minibuffer "Gist: ")))))

(defun search-google-scholar (&optional arg)
  "Search selected region online.
With a prefix ARG, enclose region in quotes. If nothing is selected,
prompt for a string in the minibuffer."
  (interactive "P")
  (let ((str (url-hexify-string (buffer-substring
				 (region-beginning)
				 (region-end)))))
    (browse-url
     (format
      "https://scholar.google.co.uk/scholar?q=%s"
      (if (use-region-p)
	  (if arg
	      (concat "\"" (url-unhex-string str) "\"")
	    str)
	(read-from-minibuffer "Google Scholar: "))))))

(provide 'config-search)
;;; config-search.el end here
