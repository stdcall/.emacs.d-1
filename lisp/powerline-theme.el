;;; powerline-theme.el --- A very simple powerline theme  -*- lexical-binding: t; -*-

;;; Code:

(defun powerline-custom-theme ()
  "Setup custom modeline."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active nil nil))
                          (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l) ; print %, * or -
                                     ;; (when powerline-display-buffer-size
                                     ;;   (powerline-buffer-size nil 'l)) ; buffer size
                                     ;; (when powerline-display-mule-info
                                     ;;   (powerline-raw mode-line-mule-info nil 'l))
				     ;; (powerline-raw " ")
                                     (powerline-buffer-id nil 'l) ; buffer/file name
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode nil 'l)
                                     (powerline-process nil)
                                     (powerline-minor-modes nil 'l)
                                     (powerline-narrow nil 'l)
                                     (powerline-raw " ")
                                     (funcall separator-left nil nil)
                                     (powerline-vc nil 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) nil 'l))))
                          (rhs (list (powerline-raw global-mode-string nil 'r)
                                     (funcall separator-right nil nil)
				     (unless window-system
				       (powerline-raw (char-to-string #xe0a1) nil 'l))
				     (powerline-raw "%4l" nil 'l) ; current line number
				     (powerline-raw ":" nil 'l)
				     ;; (powerline-raw "%3c" nil 'r) ; column number
				     (funcall separator-right nil mode-line)
				     (powerline-raw " ")
				     (powerline-raw "%5p" nil 'r) ; percentage of buffer
                                     (when powerline-display-hud
                                       (powerline-hud nil nil)))))
		     (concat (powerline-render lhs)
			     (powerline-fill face2 (powerline-width rhs))
			     (powerline-render rhs)))))))

(provide 'powerline-theme)
;;; powerline-theme.el ends here

