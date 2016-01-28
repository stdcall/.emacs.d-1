;;; config-emms.el --- emacs multi-media system configuration file

;;; Installation:

;; Add this file to your load-path and require it. You will also need
;; mplayer and taglib installed on your system. Consider adding
;; af=scaletempo to your .mplayer config file located in your home
;; directory. This prevents the pitch from changing when the playback
;; speed is altered.

;;; Commentary:

;; There are a few useful commands in the playlist buffer: C
;; emms-playlist-clear; E emms-tag-editor-edit; S t sort by title; S a
;; sort by artist; S N sort by name.
;;
;; To delete a file from disk you will have to (1) S-m to enable
;; emms-mark-mode; (2) d to open the file in a dired buffer; (3) d to
;; delete the file; and (4) x to execute.

;;; Code:

(require 'emms-setup)
(emms-standard)
(emms-default-players)
(require 'emms-player-mplayer)
(require 'emms-player-simple)
(require 'emms-mode-line)
(emms-mode-line 1)
(require 'emms-mode-line-icon)
(require 'emms-info)
(require 'emms-playing-time)
(emms-playing-time 1)
(require 'emms-tag-editor)
(require 'emms-playlist-sort)
(require 'emms-bookmarks)
(require 'emms-streams)
(require 'emms-mark)
(require 'helm-emms)

(defgroup config-emms nil
  "Customization group for `config-emms'.")

(defcustom emms-seconds-to-rewind -5
  "Number of seconds to rewind. The default value is -5."
  :type 'number
  :group 'config-emms)

(defcustom emms-seconds-to-fast-forward +5
  "Number of seconds to fast forward. The default value is +5."
  :type 'number
  :group 'config-emms)

(defvar emms-player-mplayer-speed-increment 0.1)

(setq emms-playing-time-display-format "%s")
(setq emms-tag-editor-rename-format "%t")
(setq emms-show-format "%s")
(setq emms-player-list '(emms-player-mplayer-playlist
			 emms-player-mplayer
			 emms-player-mpg321))
(setq emms-source-file-default-directory "~/Music/")
(setq emms-player-mplayer-command-name "mplayer")

(add-hook 'emms-playlist-mode-hook 'hl-line-mode)

(require 'emms-info-libtag)
(setq emms-info-functions '(emms-info-libtag))
(setq emms-info-libtag-program-name "/usr/bin/emms-print-metadata")

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

(defun emms-open-music-directory ()
  (interactive)
  (dired emms-source-file-default-directory))

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

(defun emms-default-playlist ()
  "Add music library to the playlist and go to it."
  (interactive)
  (emms-add-directory-tree emms-source-file-default-directory)
  (emms))

;; add tracks from dired

(defun dired-add-to-emms-playlist nil
  "Add directory tree to the current EMMS playlist."
  (interactive)
  (let ((file (expand-file-name (dired-get-filename))))
    (if (and file (file-directory-p file))
        (emms-add-directory-tree file)
      (emms-add-file file))))

(add-hook 'dired-mode-hook 'my-dired-mode-hook)

(defun my-dired-mode-hook()
  (define-key dired-mode-map (kbd "e") 'dired-add-to-emms-playlist))

;; emms track display

(defun jag/emms-track-description (track)
  (let* ((empty "...")
	 (name (emms-track-name track))
	 (type (emms-track-type track))
	 (short-name (file-name-nondirectory name))
	 (artist (or (emms-track-get track 'info-artist) empty))
	 (year (or (emms-track-get track 'info-year) empty))
	 (ptime (or (emms-track-get track 'info-playing-time) 0))
	 (short-name (file-name-sans-extension
		      (file-name-nondirectory name)))
	 (title (or (emms-track-get track 'info-title) short-name))
	 )
    (format "%-46s | %-46s | %2d:%02d | %2s"
	    title
	    artist
	    (/ ptime 60)
	    (% ptime 60)
	    year)))

(setq emms-track-description-function 'jag/emms-track-description)

;; emms modeline display
;; http://metasyntax.net/guides/emacs.html

(setq emms-mode-line-mode-line-function
      (lambda nil
	(let ((track (emms-playlist-current-selected-track)))
          (let ((title (emms-track-get track 'info-title)))
            (if (not (null title))
		(format emms-mode-line-format title)
              (if (not (null (string-match "^url: " (emms-track-simple-description track))))
                  (format emms-mode-line-format "Internet Radio") ;FIXME:
                (format emms-mode-line-format "Unknown")))))))

;; emms volume control
;; see http://anthony.lecigne.net/2014-08-16-emms-mplayer.html

(defun jag/emms-volume-change-function (amount)
  (interactive "p")
  (process-send-string
   emms-player-simple-process-name (format "volume %d\n" amount)))

(setq emms-volume-change-function 'jag/emms-volume-change-function)

;; emms tag editor
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
  (other-window 0)
  (delete-other-windows))

;; key bindings

(global-set-key (kbd "M-p h") 'helm-emms)
(global-set-key (kbd "M-p e") 'emms)
(global-set-key (kbd "M-p RET") 'emms-add-file)
(global-set-key (kbd "M-p f") 'emms-add-directory)
(global-set-key (kbd "M-p p") 'emms-start)
(global-set-key (kbd "M-p s") 'emms-stop)
(global-set-key (kbd "M-p SPC") 'emms-pause)
(global-set-key (kbd "M-p ]") 'emms-next)
(global-set-key (kbd "M-p [") 'emms-previous)

(global-set-key (kbd "M-p a") 'emms-bookmarks-add)
(global-set-key (kbd "M-p ,") 'emms-bookmarks-prev)
(global-set-key (kbd "M-p .") 'emms-bookmarks-next)
(global-set-key (kbd "M-p c") 'emms-bookmarks-clear)

(global-set-key (kbd "M-p o") 'emms-show)
(global-set-key (kbd "M-p x") 'emms-shuffle)
(global-set-key (kbd "M-p r") 'emms-toggle-repeat-track)
(global-set-key (kbd "M-p R") 'emms-toggle-repeat-playlist)

(global-set-key (kbd "M-p =") 'emms-volume-mode-plus)
(global-set-key (kbd "M-p -") 'emms-volume-mode-minus)

(define-key emms-playlist-mode-map (kbd "+") 'emms-volume-raise)
(define-key emms-playlist-mode-map (kbd "-") 'emms-volume-lower)
(define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
(define-key emms-playlist-mode-map (kbd "M") 'emms-mark-mode)
(define-key emms-playlist-mode-map (kbd "j") 'swiper)

(define-key emms-tag-editor-mode-map (kbd "C-.") 'emms-tag-editor-next-same-field)
(define-key emms-tag-editor-mode-map (kbd "C-,") 'emms-tag-editor-prev-same-field)

(global-set-key (kbd "M-p j") 'emms-rewind)
(global-set-key (kbd "M-p k") 'emms-fast-forward)
(global-set-key (kbd "M-p m") 'emms-open-music-directory)
(global-set-key (kbd "M-p l") 'emms-default-playlist)

(global-set-key (kbd "M-p v") 'emms-player-mplayer-slow-down)
(global-set-key (kbd "M-p ^") 'emms-player-mplayer-speed-up)

(define-key emms-tag-editor-mode-map (kbd "C-c C-c") 'emms-tag-editor-submit-and-exit-2)

(provide 'config-emms)
;;; config-emms.el ends here
