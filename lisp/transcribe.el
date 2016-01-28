;;; transcribe.el --- a tool for transcribing audio with ExpressScribe in Emacs -*- lexical-binding: t; -*-

;;; Comentary:

;; ExpressScribe uses the following key bindings:

;; | C-d   | rewind                       |
;; | C-j   | play                         |
;; | C-k   | stop                         |
;; | C-M-j | decrease playback speed (5%) |
;; | C-M-k | resent playback speed (100%) |
;; | C-i   | copy time                    |
;; | M-j   | play slow speed              |
;; | M-k   | play real speed              |

;;; Code:

(require 'key-chord)
(require 'auto-capitalize)
(key-chord-mode 1)
(add-hook 'transcribe-hook #'auto-capitalize-mode)

(defgroup transcribe nil
  "Customization group for transcribe minor mode.")

(define-minor-mode transcribe
  "A minor mode for making transcriptions."
  :lighter " Transcribe"
  :keymap (let ((map (make-sparse-keymap)))
            map))

(key-chord-define transcribe-map "jk" 'transcribe-insert-interviewer)
(key-chord-define transcribe-map "jl" 'transcribe-insert-interviewee)
(key-chord-define transcribe-map ".," 'transcribe-insert-ellipsis)

(defcustom transcribe-interviewer nil
  "The name of the interviewer."
  :type 'string
  :group 'transcribe)

(defcustom transcribe-interviewee nil
  "The name of the interviewee."
  :type 'string
  :group 'transcribe)

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
  "Insert ellipsis."
  (interactive)
  (insert "(...) "))

(defun transcribe-open-gui ()
  "Open ExpressScribe."
  (interactive)
  (shell-command-to-string "open -a ExpressScribe"))

(defun transcribe-initialize ()
  "Enable `transcribe' settings and open GUI."
  (interactive)
  (transcribe)
  (transcribe-open-gui))

(defun transcribe-insert-link ()
  "Insert link that opens the GUI."
  (interactive)
  (insert "elisp:transcribe-initialize"))

(defun transcribe-new-name ()
  "Prompt for a new name, replacing all occurances of
\"Interviewee\"."
  (interactive)
  (let ((new-name (read-from-minibuffer "Enter new name: ")))
    (insert (format "%s" new-name
		    (let ((case-fold-search t))
		      (goto-char (point-min))
		      (while (search-forward "Interviewee" nil t)
			(replace-match new-name t t)))))))

(font-lock-add-keywords 'org-mode
                        '(("\\[[0-9]:[0-9][0-9]:[0-9][0-9]]" . font-lock-constant-face)))

(font-lock-add-keywords 'org-mode
                        '(("Jonathan:" . font-lock-constant-face)))

(font-lock-add-keywords 'org-mode
                        '(("Interviewee:" . font-lock-variable-name-face)))

(setq auto-capitalize-predicate
      (lambda () (not (looking-back
		       "\\([Ee]\\.g\\|[Ii]\\.e\\|\\.\\.\\)\\.[^.]*" (- (point) 20)))))

(provide 'transcribe)
;;; transcribe ends here
