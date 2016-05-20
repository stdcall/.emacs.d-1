;;; config-agenda.el --- org-agenda custom commands

(setq org-agenda-custom-commands
      '(("x" "appts" agenda*)

	("a" "agenda"
	 ((agenda ""
		  ((org-agenda-prefix-format " %i %-12:c%?-12t% s %e ")
		   (org-agenda-remove-tags t)))
	  (tags "LEVEL=2+CATEGORY=\"task\"/!"
		((org-agenda-overriding-header "Unscheduled Tasks")
		 (org-agenda-compact-blocks nil)
		 (org-agenda-skip-function '(org-agenda-skip-entry-if
					     'todo '("NEXT" "WAITING" "CANCELED" "DEFERRED")
					     'deadline 'scheduled 'timestamp))))
	  (tags-todo "LEVEL=2|LEVEL=3-CATEGORY={practice}/!NEXT"
		     ((org-agenda-overriding-header "Next Action")
		      (org-agenda-compact-blocks t))))
	 ((org-agenda-remove-tags nil)
	  (org-agenda-show-inherited-tags nil)
	  (org-agenda-prefix-format " %i %-10:c %-4e ")))

	("w" "review"
	 ((todo "WAITING"
		((org-agenda-overriding-header "Waiting for")))
	  (tags-todo "-CANCELED/!"
		     ((org-agenda-overriding-header "Stuck Projects")
		      (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
          (tags "TODO=\"DONE\"-exclude-CATEGORY={practice\\|garden}|+TODO=\"CANCELED\""
                ((org-agenda-overriding-header "Tasks to Refile"))) ; to refile/archive in bulk, press m B
	  (tags "LEVEL=2+CATEGORY=\"someday\""
		((org-agenda-overriding-header "Someday/Maybe"))))
	 ((org-agenda-remove-tags nil)
	  (org-agenda-show-inherited-tags nil)
	  (org-agenda-skip-function nil)
	  (org-agenda-compact-blocks t)
	  (org-agenda-files '("~/org/todo.org"))))

        ("n" "notes"
         ((tags "LEVEL=2+CATEGORY=\"notes\""
		((org-agenda-overriding-header "Notes")
		 (org-agenda-prefix-format "  ")))))

        ("c" . "context")

	("cc" "physical context (@errands, @phone, @mail, @home)"
         ((tags-todo "errands-CATEGORY=\"tickler\""
                     ((org-agenda-overriding-header "@errands")))
          (tags-todo "phone"
                     ((org-agenda-overriding-header "@phone")))
	  (tags-todo "mail"
		     ((org-agenda-overriding-header "@mail")))
	  (tags-todo "home"
		     ((org-agenda-overriding-header "@home")))))

	("cr" "read" tags "read"
	 ((org-agenda-overriding-header "To Read")))
	("cw" "watch" tags "watch"
	 ((org-agenda-overriding-header "To Watch")))
        ("cs" "see" tags "see"
	 ((org-agenda-overriding-header "To See")))
	("ci" "listen" tags "listen"
	 ((org-agenda-overriding-header "To Listen")))
        ("cl" "learn" tags "learn"
	 ((org-agenda-overriding-header "To Learn")))
        ("ct" "think" tags "think"
	 ((org-agenda-overriding-header "To Think")))
	("ch" "hold"
	 tags "+hold|+CATEGORY=\"reference\"|+CATEGORY=\"bookmark\"")

        ("ca" "contacts"
         ((tags-todo "LEVEL=2+CATEGORY=\"contacts\"+TODO=\"MEET\""
                     ((org-agenda-overriding-header "Unscheduled")
		      (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
          (tags-todo "LEVEL=2+CATEGORY=\"contacts\""
                     ((org-agenda-overriding-header "Scheduled")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'notscheduled))))
          (tags-todo "LEVEL=2+CATEGORY=\"contacts\"+TODO=\"WAITING\""
                     ((org-agenda-overriding-header "Waiting")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
          (tags-todo "LEVEL=2+CATEGORY=\"contacts\"+TODO=\"EMAIL\""
                     ((org-agenda-overriding-header "Email")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
          (tags-todo "LEVEL=2+CATEGORY=\"contacts\"+TODO=\"CALL\""
                     ((org-agenda-overriding-header "Call")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
          (tags "LEVEL=2+CATEGORY=\"contacts\"+TODO=\"FOLLOW-UP\""
                ((org-agenda-overriding-header "Follow-up")))
          (tags "LEVEL=2+CATEGORY=\"contacts\"+TODO=\"DONE\""
                ((org-agenda-overriding-header "DONE")
                 (org-agenda-skip-function nil)))
          (tags "LEVEL=2+CATEGORY=\"contacts\""
                ((org-agenda-overriding-header "Contacts")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if
                                             'todo '("MEET" "WAITING" "EMAIL" "CALL" "FOLLOW-UP" "DONE")))))))

        ("p" . "print")			; press C-a a e to export

        ("pa" "agenda"
         ((agenda "" ((org-agenda-ndays 2)))
          (tags-todo "errands-CATEGORY=\"tickler\""
                     ((org-agenda-prefix-format "  [ ]")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-overriding-header "\n@errands:")))
          (tags-todo "phone"
                     ((org-agenda-prefix-format "  [ ]")
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-overriding-header "\n@phone:"))))
         ((org-agenda-with-colors nil)
          (org-habit-show-habits nil)
	  (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t))
         ("~/ownCloud/agenda.txt"))

        ("pg" "groceries"
	 ((tags "LEVEL=3+CATEGORY=\"vegetables\""
		((org-agenda-prefix-format "  [ ] ")
		 (org-agenda-overriding-header
		  (concat (format-time-string "%A    %d %B %Y%n" (current-time))
			  "\nFruits & Vegetables:"))))
          (tags "LEVEL=3+CATEGORY=\"seeds\""
                ((org-agenda-prefix-format "  [ ] ")
                 (org-agenda-overriding-header "\nSeeds & nuts:")))
          (tags "LEVEL=3+CATEGORY=\"general\""
                ((org-agenda-prefix-format "  [ ] ")
                 (org-agenda-overriding-header "\nGeneral:")))
          (tags "LEVEL=3+CATEGORY=\"other\""
                ((org-agenda-prefix-format "  [ ] ")
                 (org-agenda-overriding-header "\nOther:"))))
	 ((org-agenda-with-colors nil)
	  (org-agenda-sorting-strategy '(tag-up))
	  (org-agenda-tags-column -30)
	  (org-agenda-show-inherited-tags nil)
	  (org-agenda-remove-tags nil)
	  (org-agenda-compact-blocks t))
	 ("~/ownCloud/groceries.txt"))

        ("r" "practice"
         ((tags "LEVEL>=3+CATEGORY=\"practice\"-exclude+next"
                ((org-agenda-overriding-header "Practice!")
                 (org-agenda-prefix-format "  %-10:c %-8e")))))))

;; generate reports

(add-to-list 'org-agenda-custom-commands
             '("D" "daily report"
               ((agenda ""))
               ((org-agenda-overriding-header "Daily report")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp
                                                                     'regexp ":exclude:"))
                (org-agenda-span 1)
                (org-agenda-show-log t)
                (org-agenda-log-mode-items '(closed))
                (org-agenda-archives-mode t)
                (org-agenda-start-with-clockreport-mode t)
                (org-agenda-time-grid nil))) t)

(add-to-list 'org-agenda-custom-commands
             '("W" "weekly report"
               ((agenda ""))
               ((org-agenda-overriding-header "Weekly report")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                (org-agenda-span 8)
                (org-agenda-start-day "-7d")
                (org-agenda-show-log t)
                (org-agenda-log-mode-items '(closed))
                (org-agenda-archives-mode t)
                (org-agenda-start-with-clockreport-mode t)
                (org-agenda-time-grid nil))) t)

(add-to-list 'org-agenda-custom-commands
	     '("h" "habit"
	       ((agenda ""))
	       ((org-agenda-overriding-header "Habit report")
		(org-agenda-prefix-format " %i %-2:c%?-12t% s %e ")
		(org-agenda-ndays 1)
		(org-habit-show-all-today t)
		(org-habit-graph-column 45)
		(org-habit-preceding-days 60)
		(org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "habit")))))

(provide 'config-agenda)
;;; config-agenda.el ends here
