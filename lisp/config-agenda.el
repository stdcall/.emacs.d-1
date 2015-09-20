;;; config-agenda.el --- org-agenda custom commands

(setq org-agenda-custom-commands
      '(
	("a" "agenda"
	 ((agenda ""
		  ((org-agenda-prefix-format " %i %-12:c%?-12t% s %e ")))
	  (tags-todo "+PRIORITY=\"A\"|TODO=\"STARTED\"")
	  (tags-todo "mail/TODO"
	  	     ((org-agenda-skip-function '(org-agenda-skip-entry-if
						  'deadline 'scheduled 'timestamp))))
	  ))

	("x" "next action"
         ((tags-todo "LEVEL=2-SCHEDULED=\"<today>\"/NEXT"
                     ((org-agenda-overriding-header "Next action")
                      (org-agenda-sorting-strategy
                       '(priority-down effort-up))))
	  (tags-todo "LEVEL<=2+Effort<=\"0:20\"+Effort=>\"0:02\"-CATEGORY={someday\\|practice\\|habit}/TODO"
                     ((org-agenda-overriding-header "Low energy")
                      (org-agenda-sorting-strategy
                       '(effort-up))))
          (tags-todo "LEVEL>=3-CATEGORY={someday\\|practice}-SCHEDULED=\"<today>\"-errands/TODO"
                     ((org-agenda-overriding-header "Sub-tasks")
                      (org-agenda-sorting-strategy
                       '(priority-down todo-state-up)))))
         ((org-agenda-remove-tags nil)
          (org-agenda-show-inherited-tags nil)
          (org-agenda-tags-column -110)
          (org-agenda-prefix-format "  %-40b %-6e ")))

        ("i" "inbox"                    ; weekly review
         ((tags "LEVEL=2+CATEGORY=\"task\"/!"
                ((org-agenda-overriding-header "Unscheduled Tasks")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if
                                             'todo '("NEXT" "WAITING" "CANCELED" "DEFERRED")
                                             'deadline 'scheduled 'timestamp))))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting for")))
          (tags-todo "LEVEL=2+CATEGORY=\"stuck\"-TODO=\"WAITING\""
                     ((org-agenda-overriding-header "Stuck Tasks")))
          (tags "+SCHEDULED<\"<today>\"-CATEGORY=\"habit\"|+DEADLINE<\"<today>\"|+TIMESTAMP<\"<today>\"-CATEGORY=\"appt\""
                ((org-agenda-overriding-header "Past due")))
          (tags "LEVEL=2+CATEGORY=\"appt\"+TIMESTAMP<\"<today>\"-repeat"
                ((org-agenda-overriding-header "Past appointments")
                 (org-agenda-skip-function nil)))
          (tags "TODO=\"DONE\"-exclude-CATEGORY={appt\\|practice}|+TODO=\"CANCELED\""
                ((org-agenda-overriding-header "Tasks to Refile") ;to archive, press m B $
                 (org-agenda-skip-function nil))))
         ((org-agenda-remove-tags nil)
          (org-agenda-tags-column -110)
          (org-agenda-show-inherited-tags nil)))

        ("o" "someday"
         ((tags "LEVEL=2+CATEGORY=\"someday\""
                ((org-agenda-overriding-header "Someday/Maybe")
                 (org-agenda-sorting-strategy 
                  '(todo-state-up))))
          (tags "LEVEL>=3+CATEGORY=\"someday\""
                ((org-agenda-sorting-strategy 
                  '(todo-state-up))))))
                
        ("n" "notes"
         ((tags-todo "LEVEL=2+CATEGORY=\"notes\""
                     ((org-agenda-overriding-header "Notes")))
          (tags-todo "LEVEL=2+CATEGORY=\"meeting\""
                     ((org-agenda-overriding-header "Meetings tasks"))))
         ((org-agenda-remove-tags nil)
          (org-agenda-show-inherited-tags nil)
          (org-agenda-tags-column -112)
          (org-agenda-prefix-format "  ")))

        ("c" . "context")
	;; physical context
	("cc" "physical context (@errands, @phone, @mail, @home)"
         ((tags-todo "errands-CATEGORY=\"tickler\""
                     ((org-agenda-overriding-header "@errands")))
          (tags-todo "phone"
                     ((org-agenda-overriding-header "@phone")))
	  (tags-todo "mail"
		     ((org-agenda-overriding-header "@mail")))
	  (tags-todo "home"
		     ((org-agenda-overriding-header "@home")))
	  ))
	;; perceptual context
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
                                             'todo '("MEET" "WAITING" "EMAIL" "CALL" "FOLLOW-UP" "DONE")))))
          ))

        ("p" . "print")			;press C-a a e to export
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
	  (org-agenda-compact-blocks t))
	  ("~/ownCloud/groceries.txt"))

        ;; ("pi" "interview"
        ;;  ((tags-todo "LEVEL=3+CATEGORY=\"query\"+next-exclude"
        ;;              ((org-agenda-overriding-header "Interview:")))
        ;;   (tags-todo "query-exclude"
        ;;              ((org-agenda-overriding-header "Queries:\n"))))
        ;;  ((org-agenda-with-colors nil)
        ;;   (org-agenda-prefix-format "\n-")
        ;;   (org-agenda-todo-keyword-format ""))
        ;;  ("~/ownCloud/interview.txt"))

        ("P" "practice"
         ((tags "LEVEL>=3+CATEGORY=\"practice\"-exclude+next"
                ((org-agenda-overriding-header "Practice!")
                 (org-agenda-prefix-format "  %-10:c %-8e")))))
        ))

;; generate reports

(add-to-list 'org-agenda-custom-commands
             '("d" "daily report" ;TODO: make timestamp inactive when
				  ;item is closed
               ((agenda ""))
               ((org-agenda-overriding-header "Daily report\n------------")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp
                                                                     'regexp ":exclude:"))
                (org-agenda-span 1)
                (org-agenda-show-log t)
                (org-agenda-log-mode-items '(closed))
                (org-agenda-archives-mode t)
                (org-agenda-start-with-clockreport-mode t)
                (org-agenda-time-grid nil))) t)

(add-to-list 'org-agenda-custom-commands
             '("w" "weekly report"
               ((agenda ""))
               ((org-agenda-overriding-header "Weekly report\n-------------")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                (org-agenda-span 8)
                (org-agenda-start-day "-7d")
                (org-agenda-show-log t)
                (org-agenda-log-mode-items '(closed))
                (org-agenda-archives-mode t)
                (org-agenda-start-with-clockreport-mode t)
                (org-agenda-time-grid nil))) t)

(provide 'config-agenda)
