;;; config-mu4e.el --- mail client for emacs

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path "~/.emacs.d/emacs-async")

(require 'mu4e)
(require 'smtpmail)
(require 'starttls)
(require 'org-mu4e)
(require 'gnus-dired)
(require 'smtpmail-async)

(setq mu4e-maildir "~/Maildir")
(setq mu4e-sent-folder   "/sent"
      mu4e-drafts-folder "/drafts"
      mu4e-trash-folder  "/trash"
      mu4e-refile-folder "/archive")

;; accounts

(defvar my-mu4e-account-alist
  '(("ai"
     (user-mail-address "xxxx@autistici.org")
     (smtpmail-smtp-server "smtp.autistici.org"))
    ("uni"
     (user-mail-address "xxxx@qub.ac.uk")
     (smtpmail-smtp-user "xxxx@ads.qub.ac.uk")
     (smtpmail-starttls-credentials '(("smtp.office365.com" 587 nil nil)))
     (smtpmail-smtp-server "smtp.office365.com"))))

;; send mail

(setq send-mail-function 'async-smtpmail-send-it
      message-send-mail-function 'async-smtpmail-send-it)

(setq smtpmail-starttls-credentials '(("smtp.autistici.org" 587 nil nil))
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      smtpmail-smtp-server "smtp.autistici.org"
      smtpmail-default-smtp-server "smtp.autistici.org"
      smtpmail-smtp-service 587)

;; maildirs frequently used; access them with 'j' ('jump')

(setq mu4e-maildir-shortcuts
      '(("/sent" .    ?s)
	("/drafts" .  ?d)
	("/trash" .   ?t)
	("/archive" . ?a)))

;; program to get mail

(setq mu4e-get-mail-command "getmail --rcfile getmailrc --rcfile getmailrc2")
(setq mu4e-update-interval (* 10 60))	;update every 10 minutes

;; signature

(setq mu4e-compose-signature "Jonathan")
(setq mu4e-compose-signature-auto-include t)

(defun jag/message-goto-body ()
  "Similar to `message-goto-body' but moves point above the
signature line."
  (interactive)
  (goto-char (point-max))
  (when (re-search-backward "--"))
  (newline)
  (previous-line))

(define-key mu4e-compose-mode-map (kbd "C-c C-b") 'jag/message-goto-body)

;; general settings

(setq message-kill-buffer-on-exit t)
(setq mu4e-attachment-dir  "~/private/tmp")
(setq mu4e-confirm-quit nil)
(setq mu4e-headers-skip-duplicates t)
(setq mu4e-compose-dont-reply-to-self t)
(setq mu4e-view-show-addresses t)
(setq mu4e-view-show-images nil)
(setq mu4e-date-format-long "%Y-%m-%d %H:%M")
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
(setq mu4e-html2text-command "w3m -I utf8 -O utf8 -T text/html")

;; define actions

(setq mu4e-view-actions			;press a
      '(("capture action" . jag/mu4e-capture-message)
	("appt" . jag/mu4e-capture-appt)
	;; ("capture message" . mu4e-action-capture-message)
	;; ("view as pdf" . mu4e-action-view-as-pdf) ;TODO: install msg2pdf
	("tag" . mu4e-action-retag-message)
	("hold" . mu4e-action-tag-hold)
	("untagHold" . mu4e-action-untag-hold)
	("search sender" . search-for-sender)
	("view in browser" . mu4e-action-view-in-browser))) ;FIXME: export encoding

(setq mu4e-headers-actions
      '(("tag" . mu4e-action-retag-message)
	("hold" . mu4e-action-tag-hold)
	("untagHold" . mu4e-action-untag-hold)))

(defun mu4e-action-tag-hold (msg)
  "Add a `hold' tag to the current message and prompt for a
'tickler' date."
  (org-capture nil "#")
  (mu4e-action-retag-message msg "+hold"))

(defun mu4e-action-untag-hold (msg)
  "Remove `hold' tag from the current message."
  (mu4e-action-retag-message msg "-hold"))

(defun search-for-sender (msg)
  "Search for messages sent by the sender of the message at
point."
  (mu4e-headers-search
   (concat "from:" (cdar (mu4e-message-field msg :from)))))

(defun jag/mu4e-capture-message (msg)
  "Capture an action and link it to the original message. The
function uses`org-capture' and adds the task to the agenda."
  (org-capture nil "&"))

(defun jag/mu4e-capture-appt (msg)
  "Capture an appointment and link it to the original message. The
function uses`org-capture' and adds the task to the agenda."
  (org-capture nil "^"))

;; enable spell checking when composing a message

(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

;; enable encryption; C-c C-e s (sign); C-c C-e e (encrypt); C-c C-e v
;; (verify); C-c C-e d (decrypt)

(add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
(add-hook 'mu4e-view-mode-hook 'epa-mail-mode)
(define-key mu4e-view-mode-map (kbd "C-c C-e v") 'epa-mail-verify)
(define-key mu4e-view-mode-map (kbd "C-c C-e d") 'epa-mail-decrypt)

;; 1) messages to me@foo.com should be replied with From:me@foo.com
;; 2) messages to me@bar.com should be replied with From:me@bar.com
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
		       (t "xxxx@autistici.org")))))))

;; enable notification with `terminal-notifier'

(add-hook 'mu4e-index-updated-hook
          (lambda ()
            (let ((msg (newest-subject)))
              (unless (string-equal ": " msg)
                (shell-command (concat "terminal-notifier -title \"New message\" -sender \"org.gnu.Emacs\" -sound 'default' -message \"" msg "\"")))) ))

(defun newest-subject ()		;TODO: display number of unread messages
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

;; the headers to show in the headers list -- a pair of a field and
;; its width, with `nil' meaning 'unlimited' (better only use that for
;; the last field.

(setq mu4e-headers-fields
      '( (:date           .  27)    ;; alternatively, use :human-date
	 (:flags          .   6)
	 (:from-or-to     .  22)
	 (:thread-subject .  nil))) ;; alternatively, use :subject

;; function that takes a msg and returns a string for the description
;; part of an org-mode link.

(defun my-link-descr (msg)
  (let ((subject (or (plist-get msg :subject)
		     "No subject")))
    (concat subject "")))

(setq org-mu4e-link-desc-func 'my-link-descr)

;; email selected region

(defun mu4e-email-region (start end)
  "Send region as the body of an email."
  (interactive "r")
  (let ((content (buffer-substring start end)))
    (mu4e-compose-new)
    (message-goto-body)
    (insert (concat content "\n"))
    (message-goto-to)))

;; auto fill can sometimes break long web links into multiple lines.
;; Shortening the URL solves this problem.

(defun jag/mu4e-shorten-url (url)
  "Shorten URLs using https://is.gd back end. See
https://is.gd/apishorteningreference.php for additional
parameters."
  (interactive "sShorten URL: ")
  (let ((buf (url-retrieve-synchronously
	      (format "http://is.gd/create.php?format=simple&url=%s" url))))
    (save-excursion
      (set-buffer buf)
      (let ((beg (point)))
	(goto-line 11)
      	(kill-region (point) beg))
      (switch-to-buffer (other-buffer (current-buffer) t))
      (yank))))

;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode

(defun gnus-dired-mail-buffers ()	;press C-c RET C-a on dired
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
	(set-buffer buffer)
	(when (and (derived-mode-p 'message-mode)
		   (null message-sent-message-via))
	  (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; key bindings

(global-set-key (kbd "∫") 'mu4e-headers-search-bookmark) ;that's alt-b, as in bookmarks
(global-set-key (kbd "ø") 'helm-mu) ;that's alt-o
(global-set-key (kbd "C-x M") 'mu4-email-region)
(define-key mu4e-compose-mode-map (kbd "C-c s") nil)
(define-key mu4e-compose-mode-map (kbd "C-c s") 'message-goto-subject)
(define-key mu4e-compose-mode-map (kbd "C-c C-x f") 'Footnote-add-footnote) ;sames as `org-footnote-action'
(define-key mu4e-compose-mode-map (kbd "C-c .") 'jag/mu4e-trim-posting)
(define-key mu4e-compose-mode-map (kbd "C-c u") 'jag/mu4e-shorten-url)
(define-key mu4e-main-mode-map (kbd "q") 'bury-buffer)
(define-key mu4e-main-mode-map (kbd "x") 'mu4e-quit)
(define-key mu4e-main-mode-map (kbd "k") 'hydra-mail/body)
(define-key mu4e-view-mode-map (kbd "l") 'hydra-scroll/body)

;; bookmarks

(setq mu4e-bookmarks
      '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
	("NOT flag:trashed AND NOT maildir:/archive AND NOT maildir:/sent" "Unprocessed messages" ?i) ;inbox
	("maildir:/archive" "Archived messages" ?a)
	("list:*" "Mailing lists" ?l)
	("date:2d..now AND NOT flag:trashed AND NOT list:*" "Last 2 days" ?d)
	("date:7d..now AND NOT flag:trashed AND NOT list:*" "Last 7 days" ?w)
	("date:30d..now AND NOT flag:trashed AND NOT list:*" "Last 30 days" ?m)
	("tag:hold OR flag:flagged" "Messages on hold" ?h) ;tag
	("flag:attach AND NOT list:*" "Messages with attachment" ?A)
	("maildir:/uni" "Messages from Queen's" ?q)
	))

(add-to-list 'mu4e-bookmarks
	     '((concat "NOT flag:trashed AND date:.."
		       (format-time-string "%Y%m%d"
					   (subtract-time (current-time) (days-to-time (* 30 2)))))
	       "Old messages" ?o) t)	;messages that are more than 2
					;months old should probably be
					;deleted

;; BBDB contact management; (kbd M-i b)
;; http://git.savannah.gnu.org/cgit/bbdb.git/plain/README
;; ./configure --with-mu4e-dir=/usr/local/share/emacs/site-lisp/mu4e

(require 'bbdb-loaddefs "~/.emacs.d/bbdb/bbdb-loaddefs.el")
(setq bbdb-file "~/.bbdb")
(setq bbdb-mail-user-agent (quote message-user-agent))
(setq mu4e-view-mode-hook (quote (bbdb-mua-auto-update visual-line-mode)))
(setq mu4e-compose-complete-addresses nil)
(setq bbdb-completion-display-record nil)
(setq bbdb-phone-label-list (quote ("work" "home" "mobile" "other")))
(setq bbdb-phone-style nil)
(setq bbdb-mua-update-interactive-p '(query . create))
(setq bbdb-message-all-addresses t)	;return all mail addresses of a message
(setq bbdb-mua-pop-up nil)

(defun jag/bbdb-next-record (n)
  "Move point to the beginning of the next BBDB record.
With prefix N move forward N records."
  (interactive "p")
  (let ((npoint (bbdb-scan-property 'bbdb-record-number 'integerp n)))
    (if npoint (goto-char npoint)
      (ignore-errors))))

(defun jag/bbdb-prev-record (n)
  "Move point to the beginning of the previous BBDB record.
With prefix N move backwards N records."
  (interactive "p")
  (jag/bbdb-next-record (- n)))

(defun jag/bbdb-maps()
  (define-key bbdb-mode-map (kbd "n") 'jag/bbdb-next-record)
  (define-key bbdb-mode-map (kbd "p") 'jag/bbdb-prev-record))

(add-hook 'bbdb-mode-hook 'jag/bbdb-maps)

;; use ; on a message to invoke BBDB interactively
;; http://blog.petitepomme.net/post/28547901478/installing-and-configuring-bbdb-3

(add-hook
 'mu4e-view-mode-hook
 (lambda ()
   (define-key mu4e-view-mode-map (kbd ";") 'bbdb-mua-edit-field)
   ))

;; use supercite for citation

(add-hook 'mail-citation-hook 'sc-cite-original)

(setq sc-nested-citation-p t)
(setq sc-citation-leader "")
(setq sc-confirm-always-p nil)
(setq sc-fixup-whitespace-p t)
(setq sc-preferred-attribution-list 
      '("x-attribution" "sc-lastchoice" "emailname"))

(setq sc-preferred-header-style 8)
(setq sc-rewrite-header-list
      '((sc-no-header)
	(sc-header-on-said)
	(sc-header-inarticle-writes)
	(sc-header-regarding-adds)
	(sc-header-attributed-writes)
	(sc-header-author-writes)
	(sc-header-verbose)
	(sc-no-blank-line-or-header)
	(mi-message-header-on-wrote)))

;; citation style
;; https://github.com/dhgxx/elisp/blob/master/mi-gnus-init.el

(defvar mi-message-safe-time-val nil
  "Nil if date string is invalid")

(defun mi-message-header-on-wrote ()
  "Similar to `sc-header-on-said', but using a different date
string. Add a new line before `sc-reference-tag-string' and
another after the header."
  (setq mi-message-safe-time-val
	(safe-date-to-time (sc-mail-field "date")))
  (let ((sc-mumble "")
	(whofrom (sc-whofrom)))
    (if whofrom
	(insert "\n" sc-reference-tag-string
		(sc-hdr "On "
			(format-time-string "%d %b %Y, at %H:%M" mi-message-safe-time-val) ", ")
		whofrom " wrote:\n\n"))))

;; add footnote support

(add-hook 'mu4e-compose-mode-hook 'footnote-mode)
;; Footnote-add-footnote	;C-! a
;; Footnote-delete-footnote 	;C-! d
;; Footnote-renumber-footnotes	;C-! r

;; trim posting
;; http://emacs-fu.blogspot.com/2008/12/some-simple-tricks-boxquote-footnote.html

(defun jag/mu4e-trim-posting ()
  "Trim selected text and replace it with an ellipsis."
  (interactive)
  (delete-region (region-beginning) (region-end))
  (insert "[...]\n\n"))

;; scroll by 4 lines

(define-key mu4e-view-mode-map (kbd "RET") 'jag/mu4e-scroll-up)
(define-key mu4e-view-mode-map (kbd "<backspace>") 'jag/mu4e-scroll-down)

(defun jag/mu4e-scroll-up ()
  "Scroll text of selected window up 4 line."
  (interactive)
  (scroll-up-1 4))

(defun jag/mu4e-scroll-down ()
  "Scroll text of selected window down 4 line."
  (interactive)
  (scroll-down-1 4))

(provide 'config-mu4e)
;;; config-mu4e.el ends here

;; TODO: change dictionary automatically based on the `to:' address
;; TODO: add contacts only of messages I send
