;;; config-emms.el --- Emacs Multi-Media System configuration file

;;; Commentary:

;; See: https://www.gnu.org/software/emms/manual/
;; Also useful: https://github.com/justbur/emacs-which-key

;;; Code:

(require 'emms-setup)
(require 'helm-emms)

;; ==================================================================
;;;; default settings
;; ==================================================================

(emms-all)

(setq emms-playlist-buffer-name "*Playlist*")
(setq emms-stream-buffer-name "*Streams*")
(setq emms-info-asynchronously nil)
(setq emms-playing-time-display-format "%s")
(setq emms-tag-editor-rename-format "%t")
(setq emms-source-file-default-directory "~/Music/")
(setq emms-player-mplayer-command-name "mplayer")
(setq emms-lyrics-dir "~/Music/lyrics"
      emms-lyrics-display-on-modeline nil)

(if (executable-find "mplayer")
    (setq emms-player-list '(emms-player-mplayer))
  (emms-default-players))

(when (executable-find "emms-print-metadata")
  (require 'emms-info-libtag)
  (setq emms-info-functions '(emms-info-libtag)))

(add-hook 'emms-playlist-mode-hook 'hl-line-mode)
(setq helm-emms-use-track-description-function t)

;; midi support

(when (executable-find "timidity")
  (add-to-list 'emms-player-list 'emms-player-timidity t))

(define-emms-simple-player timidity '(file)
  (emms-player-simple-regexp "midi" "mid" "rmi" "rcp" "r36" "g18" "g36" "mfi")
  "timidity")

;; ==================================================================
;;;; speed and seeking
;; ==================================================================

;; adding af=scaletempo to the .mplayer config file makes the pitch
;; slightly more stable when changing the playback speed

(defvar emms-player-mplayer-speed-increment 0.1)

(defcustom emms-seconds-to-rewind -5
  "Number of seconds to rewind. The default value is -5.")

(defcustom emms-seconds-to-fast-forward +5
  "Number of seconds to fast forward. The default value is +5.")

(defun emms-rewind ()
  "Rewind audio incrementally in seconds.
The value is specified in the variable `emms-seconds-to-rewind' and
should be a negative number."
  (interactive)
  (emms-seek emms-seconds-to-rewind))

(defun emms-fast-forward ()
  "Fast-forward audio incrementally in seconds.
The value is specified in `emms-seconds-to-fast-forward' and should be
a positive number."
  (interactive)
  (emms-seek emms-seconds-to-fast-forward))

(defun emms-player-mplayer-speed-up ()
  "Depends on mplayer's -slave mode."
  (interactive)
  (process-send-string
   emms-player-simple-process-name
   (format "speed_incr %f\n" emms-player-mplayer-speed-increment)))

(defun emms-player-mplayer-slow-down ()
  "Depends on mplayer's -slave mode."
  (interactive)
  (process-send-string emms-player-simple-process-name
		       (format "speed_incr %f\n" (- 0 emms-player-mplayer-speed-increment))))

;; ==================================================================
;;;; track and modeline display
;; ==================================================================

;; track display

(defun jag/emms-track-description (track)
  (let* ((empty "...")
	 (name (emms-track-name track))
	 (type (emms-track-type track))
	 (short-name (file-name-nondirectory name))
	 (artist (or (emms-track-get track 'info-artist) empty))
	 (artist-length (length artist))
	 (year (or (emms-track-get track 'info-year) "0"))
	 (ptime (or (emms-track-get track 'info-playing-time) 0))
	 (short-name (file-name-sans-extension
		      (file-name-nondirectory name)))
	 (title (or (emms-track-get track 'info-title) short-name))
	 (title-length (length title)))
    (format "%-50s %s %-50s \t %2d:%02d %2s"
	    ;; truncate long titles in the playlist buffer
	    (if (> title-length 50)
		(concat
		 (replace-regexp-in-string "[ ]*\\'" ""
					   (substring title 0 48)) "…")
	      title)
	    ;; indicate whether track has lyrics
	    (if (f-file?
		 (format "%s/%s.lrc" emms-lyrics-dir title))
		"•"			; alt-8
	      " ")
	    artist
	    (/ ptime 60)
	    (% ptime 60)
	    year)))

(setq emms-track-description-function 'jag/emms-track-description)

;; modeline display
;; http://metasyntax.net/guides/emacs.html

(defvar emms-max-title-chars 36
  "The title's maximum number of characters.
When exceeding this number, truncate the title displayed in the
modeline.")

(setq emms-mode-line-mode-line-function
      (lambda nil
	(let ((track (emms-playlist-current-selected-track)))
          (let* ((title (emms-track-get track 'info-title))
		 (length (length title))
		 (tocut (- length emms-max-title-chars))
		 (radio (cdr (nth 1 track)))
		 (fname (file-name-sans-extension
			 (file-name-nondirectory (emms-track-name track)))))
            (if (not (null title))
		(if (< emms-max-title-chars length)
		    (let ((title-short
			   (replace-regexp-in-string "[ \t\n]*\\'" ""
						     (substring title 0 (- tocut)))))
		      (format emms-mode-line-format (concat title-short "…")))
		  (format emms-mode-line-format title))
              (if (string= "streamlist" radio)
		  (let* ((url (cdr (nth 2 track)))
			 (name (replace-regexp-in-string ".*\/" "" url)))
		    (format emms-mode-line-format (concat "Radio: " (file-name-sans-extension name)))))
                (format emms-mode-line-format (if (> emms-max-title-chars (length fname))
						  fname
						(let ((fname-short
						       (replace-regexp-in-string "[ \t\n]*\\'" ""
										 (substring fname 0 (- tocut)))))
						  (concat fname-short "…")))))))))

;; ==================================================================
;;;; tag editor
;; ==================================================================

;; http://www.emacswiki.org/emacs/emms-extension.el

(defun emms-tag-editor-next-same-field (&optional reverse)
  "Jump to next same field."
  (interactive)
  (let (filed-name)
    (save-excursion
      (beginning-of-line)
      (if (search-forward-regexp "^[^ ]*[ \t]+= " (line-end-position) t)
          (setq filed-name (buffer-substring (match-beginning 0) (match-end 0)))))
    (when filed-name
      (if (null reverse)
          (search-forward-regexp filed-name (point-max) t)
        (beginning-of-line)
        (search-backward-regexp filed-name (point-min) t))
      (goto-char (match-end 0)))))

(defun emms-tag-editor-prev-same-field ()
  "Jump to previous same field."
  (interactive)
  (emms-tag-editor-next-same-field t))

(defun emms-tag-editor-submit-and-exit-2 ()
  "Submit changes made to the track information.
Exit the tag editor, delete its window and return to the playlist
buffer."
  (interactive)
  (emms-tag-editor-submit-and-exit)
  (windmove-up)
  (delete-other-windows))

;; ==================================================================
;;;; lyrics
;; ==================================================================

(add-to-list 'auto-mode-alist '("\\.lrc$" . emms-lyrics-mode))
(define-key emms-lyrics-mode-map (kbd "C-c C-q") 'emms-lyrics-mode-quit)

(defun emms-lookup-lyrics ()
  "Display lyrics of the current track.
If no file is found, lookup online."
  (interactive)
  (let* ((track (emms-playlist-current-selected-track))
	 (name (cdr (assoc 'name track)))
	 (artist (cdr (assoc 'info-artist track)))
	 (title (cdr (assoc 'info-title track)))
	 (lyrics (emms-lyrics-find-lyric title)))
    (if lyrics
	(find-file lyrics)
      (if (y-or-n-p "Add new lyric?")
	  (progn (find-file (format "%s/%s.lrc" emms-lyrics-dir title))
		 (insert (format "%s - %s\n\n" title artist))
		 (browse-url
		  (format
		   "https://duckduckgo.com/?q=letra+%s+%s" title artist)))
	(browse-url
	 (format
	  "https://duckduckgo.com/?q=letra+%s+%s" title artist))))))

(defun emms-lyrics-mode-quit ()
  "Kill lyrics buffer and resume playlist buffer."
  (interactive)
  (kill-this-buffer)
  (emms-playlist-mode-go))

;; ==================================================================
;;;; misc utils
;; ==================================================================

(defun emms-go ()
  "If playlist buffer exists, switch to it.
Otherwise add default library to the playlist and go to it."
  (interactive)
  (if (mapcar #'(lambda (buf)
		  (buffer-name buf))
	      (emms-playlist-buffer-list))
      (emms-playlist-mode-go)
    (emms-add-directory-tree emms-source-file-default-directory)
    (emms)))

(defun emms-open-music-directory ()
  "Open music directory in a dired buffer."
  (interactive)
  (dired emms-source-file-default-directory))

;; add tracks from dired

(defun dired-add-to-emms-playlist ()
  "Add directory tree to the current EMMS playlist."
  (interactive)
  (let ((file (expand-file-name (dired-get-filename))))
    (if (and file (file-directory-p file))
        (emms-add-directory-tree file)
      (emms-add-file file))))

(add-hook 'dired-mode-hook 'my-dired-mode-hook)

(defun my-dired-mode-hook ()
  (define-key dired-mode-map (kbd "e") 'dired-add-to-emms-playlist))

;; volume control
;; see http://anthony.lecigne.net/blog/2014/08/16/control-volume-mplayer-emms/

(defun jag/emms-volume-change-function (amount)
  (interactive "p")
  (process-send-string
   emms-player-simple-process-name (format "volume %d\n" amount)))

(setq emms-volume-change-function 'jag/emms-volume-change-function)

;; sort emms playlist by play-count, decreasingly

(setq emms-playlist-sort-function '(lambda ()
                                     (let ((current-prefix-arg t))
                                       (emms-playlist-sort-by-play-count))))

(defadvice emms
    (after emms-after activate)
  (emms-sort))

(defun emms-show-current-track ()
  "Return a description of the current track."
  (interactive)
  (let* ((track (emms-playlist-current-selected-track))
	 (name (cdr (assoc 'name track)))
	 (filename (file-name-nondirectory
		    (file-name-sans-extension name)))
	 ;; (play-count (cdr (assoc 'play-count track)))
	 (artist (cdr (assoc 'info-artist track)))
	 (title (cdr (assoc 'info-title track))))
    (if (and title artist)
	(message (format "%s - %s" title artist))
      (message (format "%s" filename)))))

;; ==================================================================
;;;; key bindings
;; ==================================================================

(global-set-key (kbd "M-p e") 'emms-go)
(global-set-key (kbd "M-p h") 'helm-emms)
(global-set-key (kbd "M-p b") 'emms-smart-browse)
(global-set-key (kbd "M-p m") 'emms-open-music-directory)

(global-set-key (kbd "M-p p") 'emms-start)
(global-set-key (kbd "M-p s") 'emms-stop)
(global-set-key (kbd "M-p SPC") 'emms-pause)
(global-set-key (kbd "M-p M-]") 'emms-next)
(global-set-key (kbd "M-p M-[") 'emms-previous)
(global-set-key (kbd "M-p x") 'emms-shuffle)
(global-set-key (kbd "M-p r") 'emms-toggle-repeat-track)
(global-set-key (kbd "M-p o") 'emms-show-current-track)

(global-set-key (kbd "M-p a") 'emms-bookmarks-add)
(global-set-key (kbd "M-p ,") 'emms-bookmarks-prev)
(global-set-key (kbd "M-p .") 'emms-bookmarks-next)
(global-set-key (kbd "M-p c") 'emms-bookmarks-clear)

(global-set-key (kbd "M-p =") 'emms-volume-mode-plus)
(global-set-key (kbd "M-p -") 'emms-volume-mode-minus)

(define-key emms-playlist-mode-map (kbd "+") 'emms-volume-raise)
(define-key emms-playlist-mode-map (kbd "-") 'emms-volume-lower)
(define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
(define-key emms-playlist-mode-map (kbd "M") 'emms-mark-mode)
(define-key emms-playlist-mode-map (kbd "j") 'swiper)
(define-key emms-playlist-mode-map (kbd "l") 'emms-lookup-lyrics)

(define-key emms-tag-editor-mode-map (kbd "C-.") 'emms-tag-editor-next-same-field)
(define-key emms-tag-editor-mode-map (kbd "C-,") 'emms-tag-editor-prev-same-field)
(define-key emms-tag-editor-mode-map (kbd "C-c C-c") 'emms-tag-editor-submit-and-exit-2)

(global-set-key (kbd "M-p M-j") 'emms-rewind)
(global-set-key (kbd "M-p M-k") 'emms-fast-forward)
(global-set-key (kbd "M-p u") 'emms-player-mplayer-speed-up)
(global-set-key (kbd "M-p d") 'emms-player-mplayer-slow-down)

(provide 'config-emms)
;;; config-emms.el ends here
