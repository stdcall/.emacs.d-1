;;; config-mu4e.el --- mail client for Emacs

(add-to-list 'load-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu/mu4e")

(require 'mu4e)
(require 'smtpmail)
(require 'starttls)
(require 'org-mu4e)
(require 'gnus-dired)
(require 'smtpmail-async)
(require 'supercite)
(require 'bbdb-loaddefs "~/.emacs.d/bbdb/bbdb-loaddefs.el")

;; ==================================================================
;;;; basic configuration
;; ==================================================================

(setq mu4e-maildir "~/Maildir")
(setq mu4e-sent-folder   "/sent"
      mu4e-drafts-folder "/drafts"
      mu4e-trash-folder  "/trash"
      mu4e-refile-folder "/archive")

(setq smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      smtpmail-starttls-credentials '(("smtp.autistici.org" 587 nil nil))
      smtpmail-default-smtp-server "smtp.autistici.org"
      smtpmail-smtp-server "smtp.autistici.org"
      smtpmail-smtp-user "xxxx@autistici.org"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587)

(setq send-mail-function 'async-smtpmail-send-it
      message-send-mail-function 'async-smtpmail-send-it)

(setq mu4e-get-mail-command "getmail --rcfile getmailrc --rcfile getmailrc2"
      mu4e-update-interval (* 10 60))	; update every 10 minutes

;; switch between different sets of settings

(setq mu4e-context-policy 'pick-first
      mu4e-compose-context-policy 'ask
      mu4e-contexts
      `( ,(make-mu4e-context
	   :name "alias"
	   :enter-func (lambda () (mu4e-message "alias"))
	   :match-func (lambda (msg)
			 (when msg
			   (mu4e-message-contact-field-matches msg
							       :to "xxxx@autistici.org")))
	   :vars '((user-mail-address . "xxxx@autistici.org")
		   (smtpmail-smtp-user . "xxxx@autistici.org")
		   (smtpmail-default-smtp-server . "smtp.autistici.org")
		   (smtpmail-smtp-server . "smtp.autistici.org")
		   (smtpmail-starttls-credentials '(("smtp.autistici.org" 587 nil nil)))))
	 ,(make-mu4e-context
	   :name "main"
	   :enter-func (lambda () (mu4e-message "main"))
	   :match-func (lambda (msg)
			 (when msg
			   (mu4e-message-contact-field-matches msg
							       :to "xxxx@autistici.org")))
	   :vars '((user-mail-address . "xxxx@autistici.org")
		   (smtpmail-smtp-user . "xxxx@autistici.org")
		   (smtpmail-default-smtp-server . "smtp.autistici.org")
		   (smtpmail-smtp-server . "smtp.autistici.org")
		   (smtpmail-starttls-credentials '(("smtp.autistici.org" 587 nil nil)))))
	 ,(make-mu4e-context
	   :name "uni"
	   :enter-func (lambda () (mu4e-message "uni"))
	   :match-func (lambda (msg)
			 (when msg
			   (mu4e-message-contact-field-matches msg
							       :to "xxxx@qub.ac.uk")))
	   :vars '((user-mail-address . "xxxx@qub.ac.uk")
		   (smtpmail-smtp-user . "xxxx@ads.qub.ac.uk")
		   (smtpmail-smtp-server . "smtp.office365.com")
		   (smtpmail-starttls-credentials . '(("smtp.office365.com" 587 nil nil)))))))

;; This sets `mu4e-user-mail-address-list' to the concatenation of all
;; `user-mail-address' values for all contexts. If you have other mail
;; addresses as well, you'll need to add those manually.

(setq mu4e-user-mail-address-list
      (delq nil
	    (mapcar (lambda (context)
		      (when (mu4e-context-vars context)
			(cdr (assq 'user-mail-address (mu4e-context-vars context)))))
		    mu4e-contexts)))

;; ==================================================================
;; default settings
;; ==================================================================

(setq mail-user-agent 'mu4e-user-agent)
(setq message-kill-buffer-on-exit t)
(setq mu4e-attachment-dir  "~/Desktop")
(setq mu4e-confirm-quit nil)
(setq mu4e-headers-skip-duplicates t)
(setq mu4e-compose-dont-reply-to-self t)
(setq mu4e-view-show-addresses t)
(setq mu4e-headers-date-format "%d %b %Y %R")
(setq mu4e-compose-signature "Jonathan")
(setq mu4e-html2text-command (when (fboundp 'w3m)
			       (lambda ()
				 (w3m-region (point-min) (point-max)))))

;; enable inline images

(setq mu4e-view-show-images t
      mu4e-view-image-max-width 500)

;; use imagemagick, if available

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; ==================================================================
;;;; appearance
;; ==================================================================

;; the headers to show in the headers list -- a pair of a field and
;; its width, with `nil' meaning 'unlimited' (better only use that for
;; the last field.

(setq mu4e-headers-fields
      '((:date           .  19)	;; alternatively, use :human-date
	(:flags          .   6)
	(:from-or-to     .  23)
	(:thread-subject .  nil))) ;; alternatively, use :subject

(add-hook 'mu4e-main-mode-hook
	  (defun mu4e-main-mode-font-lock-rules ()
	    "Decorate mu4e main view."
	    (save-excursion
	      (goto-char (point-min))
	      (while (re-search-forward "\\[\\([a-zA-Z;]\\{1,2\\}\\)\\]" nil t)
		(add-text-properties (match-beginning 1) (match-end 1) '(face font-lock-variable-name-face))))))

;; ==================================================================
;;;; spell checking
;; ==================================================================

;; enable spell checking when composing a message

(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

;; change dictionary language automatically

(setq mu4e-al-second-language "brasileiro")
(setq mu4e-al-common-word-list
      '("escreveu" "enviada" "que" "não" "prezado" "abç"
      	"abraço" "obrigado" "obrigada" "beijo" "bjs"))

(add-hook 'mu4e-compose-mode-hook
	  (defun mu4e-change-dictionary-language ()
	    "Change Ispell dictionary language automatically when replying.
If tests return nil, do nothing."
	    (setq x mu4e-al-contact-list)
	    (let ((msg mu4e-compose-parent-message)
		  (y (car x)))
	      (when msg
		(while x
		  (if (or (mu4e-message-contact-field-matches msg :to y)
			  (mu4e-message-contact-field-matches msg :from y)
			  (save-excursion
			    (re-search-forward (concat "\\<\\(" (regexp-opt
								 (append
								  mu4e-al-common-word-list
								  nil)) "\\)\\>") nil t)))
		      (ispell-change-dictionary mu4e-al-second-language))
		  (setq y (pop x)))))))

(defadvice message-goto-body
    (after message-goto-body-after activate)
  (mu4e-compose-change-dictionary-language))

(defun mu4e-compose-change-dictionary-language ()
  "Change Ispell dictionary language automatically when composing.
This function requires calling `message-goto-body', which is normally
bound to \\[message-goto-body] in the message buffer."
  (setq x mu4e-al-contact-list)
  (let ((y (car x)))
    (save-excursion
      (goto-char (point-min))
      (while x
	(when (re-search-forward y nil t)
	  (ispell-change-dictionary mu4e-al-second-language))
	(setq y (pop x))))))

;; ==================================================================
;;;; replying
;; ==================================================================

;; 1) messages To:me@foo.com should be replied with From:me@foo.com
;; 2) messages To:me@bar.com should be replied with From:me@bar.com
;; 3) all other mail should use From:me@foo.org

(add-hook 'mu4e-compose-pre-hook
	  (defun my-set-from-address ()
	    "Set the From address based on the To address of the original."
	    (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
	      (when msg
		(setq user-mail-address
		      (cond
		       ((mu4e-message-contact-field-matches msg :to "xxxx@autistici.org")
			"xxxx@autistici.org")
		       ((mu4e-message-contact-field-matches msg :to "xxxx@autistici.org")
			"xxxx@autistici.org")
		       ((mu4e-message-contact-field-matches msg :cc "xxxx@autistici.org")
		       	"xxxx@autistici.org")
		       ((mu4e-message-contact-field-matches msg :to "xxxx@qub.ac.uk")
			"xxxx@qub.ac.uk")
		       (t "xxxx@autistici.org")))))))

;; ==================================================================
;;;; key bindings
;; ==================================================================

(global-set-key (kbd "∫") 'mu4e-headers-search-bookmark) ; that's alt-b
(global-set-key (kbd "C-x M") 'mu4e-email-region)

(define-key mu4e-compose-mode-map (kbd "C-c s") nil)
(define-key mu4e-compose-mode-map (kbd "C-c s") 'message-goto-subject)
(define-key mu4e-compose-mode-map (kbd "C-c u") 'mu4e-shorten-url)
(define-key mu4e-compose-mode-map (kbd "C-c .") 'mu4e-trim-posting)
(define-key mu4e-compose-mode-map (kbd "C-c C-x f") 'Footnote-add-footnote)

(define-key mu4e-main-mode-map (kbd "q") 'bury-buffer)
(define-key mu4e-main-mode-map (kbd "x") 'mu4e-quit)

(define-key mu4e-view-mode-map (kbd "RET") 'jag/scroll-up)
(define-key mu4e-view-mode-map (kbd "<backspace>") 'jag/scroll-down)

;; maildirs frequently used; access them with 'j' ('jump')

(setq mu4e-maildir-shortcuts
      '(("/sent" .    ?s)
	("/drafts" .  ?d)
	("/trash" .   ?t)
	("/archive" . ?a)))

;; ==================================================================
;;;; encryption
;; ==================================================================

;;; PGP/MIME
;; C-c C-m C-s (sign)
;; C-c C-m C-e (sign and encrypt)

;;; PGP inline
;; C-c C-e s (sign)
;; C-c C-e e (encrypt)
;; C-c C-e v (verify)
;; C-c C-e d (decrypt)

(add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
(add-hook 'mu4e-view-mode-hook 'epa-mail-mode)

;; ==================================================================
;;;; bookmarks
;; ==================================================================

(setq mu4e-bookmarks
      '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
	("NOT flag:trashed AND NOT maildir:/archive AND NOT maildir:/sent" "Unprocessed messages" ?i) ; inbox
	("maildir:/archive" "Archived messages" ?a)
	("list:*" "Mailing lists" ?l)
	("date:2d..now AND NOT flag:trashed AND NOT maildir:/sent" "Last 2 days" ?d)
	("date:7d..now AND NOT flag:trashed AND NOT maildir:/sent" "Last 7 days" ?w)
	("date:30d..now AND NOT flag:trashed AND NOT maildir:/sent" "Last 30 days" ?m)
	("tag:hold OR flag:flagged" "Messages on hold" ?h)
	("flag:attach AND NOT list:*" "Messages with attachment" ?A)
	("maildir:/uni OR from:qub.ac.uk" "Messages from Queen's" ?q)))

(add-to-list 'mu4e-bookmarks
	     '((concat "NOT flag:trashed AND NOT maildir:/sent AND date:.."
		       (format-time-string "%Y%m%d"
					   (subtract-time (current-time) (days-to-time (* 30 2)))))
	       "Old messages" ?o) t)

;; ==================================================================
;;;; actions
;; ==================================================================

(setq mu4e-view-actions
      '(("capture action" . jag/mu4e-capture-message)
	("appt" . jag/mu4e-capture-appt)
	("tag" . mu4e-action-retag-message)
	("showThread" . mu4e-action-show-thread)
	("hold" . mu4e-action-tag-hold)
	("untagHold" . mu4e-action-untag-hold)
	("Search sender" . search-for-sender)
	("view in browser" . mu4e-action-view-in-browser)
	("preview attachment" . mu4e-preview-attachments)))

(setq mu4e-headers-actions
      '(("capture action" . jag/mu4e-capture-message)
	("appt" . jag/mu4e-capture-appt)
	("tag" . mu4e-action-retag-message)
	("showThread" . mu4e-action-show-thread)
	("hold" . mu4e-action-tag-hold)
	("untagHold" . mu4e-action-untag-hold)))

(defun mu4e-action-tag-hold (msg)
  "Add a \"hold\" tag to the current MSG and prompt for a tickler date."
  (org-capture nil "#")
  (mu4e-action-retag-message msg "+hold"))

(defun mu4e-action-untag-hold (msg)
  "Remove \"hold\" tag from the current MSG."
  (mu4e-action-retag-message msg "-hold"))

(defun search-for-sender (msg)
  "Search for messages sent by the sender of the MSG at point."
  (mu4e-headers-search
   (concat "from:" (cdar (mu4e-message-field msg :from)))))

(defun jag/mu4e-capture-message (msg)
  "Capture an action and link it to the original MSG."
  (org-capture nil "&"))

(defun jag/mu4e-capture-appt (msg)
  "Capture an appointment and link it to the original MSG."
  (org-capture nil "^"))

;; ==================================================================
;;;; address book
;; ==================================================================

;; BBDB contact management
;; http://git.savannah.gnu.org/cgit/bbdb.git/plain/README
;; ./configure --with-mu4e-dir=/usr/local/share/emacs/site-lisp/mu4e

(setq bbdb-file "~/.bbdb")
(setq bbdb-mail-user-agent (quote message-user-agent))
(add-hook 'mu4e-view-mode-hook '(lambda () (bbdb-mua-auto-update)))
(setq mu4e-compose-complete-addresses nil)
(setq bbdb-completion-display-record nil)
(setq bbdb-phone-label-list (quote ("work" "home" "mobile" "other")))
(setq bbdb-phone-style nil)
(setq bbdb-mua-update-interactive-p '(query . create))
(setq bbdb-message-all-addresses t) ; return all mail addresses of a message
(setq bbdb-mua-pop-up nil)

;; use ; on a message to invoke BBDB interactively
;; http://blog.petitepomme.net/post/28547901478/installing-and-configuring-bbdb-3

(add-hook
 'mu4e-view-mode-hook
 (lambda ()
   (define-key mu4e-view-mode-map (kbd ";") 'bbdb-mua-edit-field)))

;; helm interface for BBDB

(use-package helm-bbdb
  :load-path "~/git/helm-bbdb")

;; ==================================================================
;;;; citation
;; ==================================================================

;; use Supercite for citation

(add-hook 'mail-citation-hook 'sc-cite-original)
(add-hook 'mu4e-compose-mode-hook 'sc-minor-mode) ; mu4e-compose-cite-function?

(setq sc-nested-citation-p t)
(setq sc-citation-leader "")
(setq sc-confirm-always-p nil)
(setq sc-fixup-whitespace-p t)
(setq sc-preferred-attribution-list
      '("x-attribution" "sc-lastchoice" "emailname"))

;; citation style

(setq sc-preferred-header-style 0)
(setq sc-rewrite-header-list
      '((sc-message-header-on-wrote)
	(sc-message-header-on-wrote-pt)))

(defvar sc-message-safe-time-val nil
  "Nil if date string is invalid.")

(defun sc-message-header-on-wrote ()
  "On <date> at <time>, <from> wrote:"
  (setq sc-message-safe-time-val
	(safe-date-to-time (sc-mail-field "date")))
  (let ((sc-mumble "")
	(whofrom (sc-whofrom)))
    (if whofrom
	(insert "\n" (sc-hdr "On "
			     (format-time-string "%d %b %Y at %R" sc-message-safe-time-val) ", ")
		whofrom " wrote:\n\n"))))

(defun sc-message-header-on-wrote-pt ()
  "Portuguese version of `sc-message-header-on-wrote'."
  (setq sc-message-safe-time-val
	(safe-date-to-time (sc-mail-field "date")))
  (let ((sc-mumble "")
	(whofrom (sc-whofrom))
	(system-time-locale "pt_BR"))
    (if whofrom
	(insert "\n" (sc-hdr "Em "
			     (format-time-string "%d de %b de %Y às %R" sc-message-safe-time-val) ", ")
		whofrom " escreveu:\n\n"))))

;; ==================================================================
;;;; extensions
;; ==================================================================

(defun mu4e-shorten-url (url)
  "Shorten URL using https://is.gd back end.
See URL `https://is.gd/apishorteningreference.php' for additional parameters."
  (interactive "sShorten URL: ")
  (let ((buf (url-retrieve-synchronously
	      (format "https://is.gd/create.php?format=simple&url=%s" url))))
    (set-buffer buf)
    (let ((str (thing-at-point 'url)))
      (goto-line 11)
      (switch-to-buffer (other-buffer (current-buffer) t))
      (insert str))))

;; enable notification

(add-hook 'mu4e-index-updated-hook
          (lambda ()
            (let ((msg (newest-subject)))
              (unless (string-equal ": " msg)
		(alert (format "%s" msg) :title "New message")))))

(defun newest-subject ()
  (let* ((mu-res (concat "(list "
			 (shell-command-to-string "mu find maildir:'/ai' flag:unread OR maildir:'/uni' flag:unread --format=sexp")
                         ")"))
         (msgs (last (car (read-from-string mu-res)))))
    (mapconcat (lambda (msg)
                 (concat (caar (plist-get msg :from))
                         ": "
                         (plist-get msg :subject)))
               msgs
               "\n")))

;; email selected region

(defun mu4e-email-region (start end)
  "Send the text between START and END as the body of an email."
  (interactive "r")
  (let ((content (buffer-substring start end)))
    (mu4e-compose-new)
    (message-goto-body)
    (insert (concat content "\n"))
    (message-goto-to)))

;; add footnote support

(add-hook 'mu4e-compose-mode-hook 'footnote-mode)
;; Footnote-add-footnote	(C-! a)
;; Footnote-delete-footnote 	(C-! d)
;; Footnote-renumber-footnotes	(C-! r)

;; trim posting
;; http://www.palmyanoff.com/trimpost.htm

(defun mu4e-trim-posting ()
  "Trim selected text and replace it with an ellipsis."
  (interactive)
  (delete-region (region-beginning) (region-end))
  (insert "[...]\n\n"))

(defun mml-attach-file--go-to-eob (orig-fun &rest args)
  "Go to the end of buffer before attaching files."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (apply orig-fun args))))

(advice-add 'mml-attach-file :around #'mml-attach-file--go-to-eob)

(provide 'config-mu4e)
;;; config-mu4e.el ends here
