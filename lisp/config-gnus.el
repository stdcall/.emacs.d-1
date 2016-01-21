;;; config-gnus.el --- newsreader client for emacs

(require 'gnus)

(setq gnus-select-method '(nntp "news.gwene.org"))

;; (setq gnus-secondary-select-methods '((nnml "")))
;; (setq send-mail-function 'smtpmail-send-it)
;; (setq message-send-mail-real-function 'smtpmail-send-it)
;; (setq message-send-mail-real-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("mail.domain.tld" 995 nil nil))
;;       smtpmail-auth-credentials '(("smtp.domain.tld" 587 "EMAIL" nil))
;;       smtpmail-smtp-server "smtp.domain.tld"
;;       smtpmail-smtp-service 587)
;; (setq mail-sources '((pop :server "mail.domain.tld"
;; 			  :user "EMAIL"
;; 			  :password "PASSWORD")))
;; (setq gnus-group-jump-to-group-prompt "nnml:mail.misc")
;; (setq message-send-mail-real-function 'smtpmail-send-it)
;; (setq gnus-posting-styles
;;       '((".*"
;; 	 )))

(setq gnus-sum-thread-tree-indent "  ")
(setq gnus-sum-thread-tree-root "")
(setq gnus-sum-thread-tree-false-root "")
(setq gnus-sum-thread-tree-single-indent "")
(setq gnus-sum-thread-tree-vertical        "│")
(setq gnus-sum-thread-tree-leaf-with-other "├─► ")
(setq gnus-sum-thread-tree-single-leaf     "└─► ")

(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%d%}" "%3{│%}"	; date
       "  "
       "%4{%-20,20f%}"			; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)

(defun my-gnus-maps ()
  (define-key gnus-summary-mode-map (kbd "RET") 'jag/gnus-summary-scroll-other-window)
  (define-key gnus-summary-mode-map (kbd "DEL") 'jag/gnus-summary-scroll-other-window-down)
  (define-key gnus-article-mode-map (kbd "q") 'jag/gnus-move-to-summary-window-and-exit)
  (define-key gnus-summary-mode-map (kbd "C-M-k") '(lambda () (interactive) (scroll-up-1 5)))
  (define-key gnus-summary-mode-map (kbd "C-M-j") '(lambda () (interactive) (scroll-down-1 5))))

(add-hook 'gnus-summary-mode-hook 'my-gnus-maps)

(defun jag/gnus-summary-scroll-other-window ()
  "Scroll text of the other window up four lines."
  (interactive)
  (smooth-scroll/scroll-other-window 5))

(defun jag/gnus-summary-scroll-other-window-down ()
  "Scroll text of the other window down four lines."
  (interactive)
  (smooth-scroll/scroll-other-window-down 5))

(defun jag/gnus-move-to-summary-window-and-exit ()
  "Move to the summary window and exit."
  (interactive)
  (other-window 1)
  (gnus-summary-exit))

(provide 'config-gnus)
;;; config-gnus.el ends here
