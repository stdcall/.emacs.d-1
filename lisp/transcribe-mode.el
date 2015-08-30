;;; transcribe-mode.el --- a tool for transcribing audio with expressscribe in emacs -*- lexical-binding: t; -*-

;;; Comentary:

;; I set ExpressScribe to use the following key bindings:

;; | C-d   | rewind                       |
;; | C-j   | play                         |
;; | C-k   | stop                         |
;; | C-M-j | decrease playback speed (5%) |
;; | C-M-k | resent playback speed (100%) |
;; | M-i   | copy time                    |
;; | M-j   | play slow speed              |
;; | M-k   | play real speed              |

;;; Code:

(require 'key-chord)
(key-chord-mode 1)
(require 'auto-capitalize)
(add-hook 'transcribe-mode-hook #'auto-capitalize-mode)

(defgroup transcribe-mode nil
  "Customization group for transcribe minor mode.")

(define-minor-mode transcribe-mode
  "A minor mode for editing transcriptions using emms."
  :lighter " Transcribe"
  :keymap (let ((map (make-sparse-keymap)))
	    ;; (define-key map (kbd "M-t") 'transcribe-insert-ellipsis)
            map))

(key-chord-define transcribe-mode-map "jk" 'transcribe-insert-interviewer)
(key-chord-define transcribe-mode-map "jl" 'transcribe-insert-interviewee)
(key-chord-define transcribe-mode-map ".," 'transcribe-insert-ellipsis)

(defcustom transcribe-interviewer nil
  "Define the name of the interviewer."
  :type 'string
  :group 'transcribe-mode)

(defcustom transcribe-interviewee nil
  "Define the name of the interviewee."
  :type 'string
  :group 'transcribe-mode)

(defun transcribe-insert-interviewer ()
  "Insert the interviewer's name in a new paragraph."
  (interactive)
  (recenter-top-bottom 0)
  (insert (concat "\n\n" transcribe-interviewer ": ")))

(defun transcribe-insert-interviewee ()
  "Insert the interviewee's name in a new paragraph."
  (interactive)
  (recenter-top-bottom 0)
  (insert (concat "\n\n" transcribe-interviewee ": ")))

(defun transcribe-insert-ellipsis ()
  "Insert ellipsis at point."
  (interactive)
  (insert "(...) "))

(defun transcribe-open-expressscribe ()
  "Open ExpressScribe GUI."
  (interactive)
  (shell-command-to-string "open -a ExpressScribe"))

(defun transcribe-initialize ()
  "Enable `transcribe-mode' settings and open ExpressScribe."
  (interactive)
  (transcribe-mode)
  (transcribe-open-expressscribe))

(defun transcribe-insert-link ()
  "Insert link that opens ExpressScribe."
  (interactive)
  (insert "elisp:transcribe-initialize"))

;; highlight

(font-lock-add-keywords 'org-mode
                        '(("\\[[0-9]:[0-9][0-9]:[0-9][0-9]]" . font-lock-constant-face)))

(font-lock-add-keywords 'org-mode
                        '(("Jonathan:" . font-lock-constant-face)))

(font-lock-add-keywords 'org-mode
                        '(("Interviewee:" . font-lock-constant-face)))

(defun transcribe-new-name ()
  "Prompt user for a new name, replacing all occurances of
`Interviewee'."
  (interactive)
  (let ((new-name (read-from-minibuffer "Enter new name: ")))
    (insert (format "%s" new-name
		    (let ((case-fold-search t))
		      (goto-char (point-min))
		      (while (search-forward "Interviewee" nil t)
			(replace-match new-name t t)))
	    ))))

(setq auto-capitalize-predicate
      (lambda () (not (looking-back
		       "\\([Ee]\\.g\\|[Ii]\\.e\\|\\.\\.\\)\\.[^.]*" (- (point) 20)))))

(provide 'transcribe-mode)
;;; transcribe-mode end here
