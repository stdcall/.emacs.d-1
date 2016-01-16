;;; config-search.el --- search online

(key-chord-define-global
 "qq"
 (defhydra hydra-query (:hint nil :color teal)
   "
 [_j_]: duckduckgo
 [_k_]: github
 [_l_]: gist
 [_q_]: quit
"
   ("j" search-duckduckgo)
   ("k" search-github)
   ("l" search-gist)
   ("q" nil)))

(defun search-duckduckgo (arg)
  "Search selected region online. With a prefix argument, enclose
region in quotes. If nothing is selected, prompt for a string in
the minibuffer."
  (interactive "P")
  (let ((str (url-hexify-string (buffer-substring
				 (region-beginning)
				 (region-end)))))
    (browse-url
     (format
      "https://duckduckgo.com/?q=%s"
      (if (use-region-p)
	  (if arg
	      (concat "\"" str "\"")
	    str)
	(read-from-minibuffer "DuckDuckGo: "))))))

(defun search-github ()
  "Search github using emacs-lisp as the default language."
  (interactive)
  (browse-url
   (format
    "https://github.com/search?l=emacs-lisp&q=%s&type=Code&utf8=✓&"
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
    "https://gist.github.com/search?utf8=✓&q=language:emacs-lisp+%s"
    (if (use-region-p)
	(url-hexify-string (buffer-substring
			    (region-beginning)
			    (region-end)))
      (read-from-minibuffer "Gist: ")))))

(provide 'config-search)
;;; config-queries.el end here
