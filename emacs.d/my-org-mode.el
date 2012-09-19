; Largely derived from http://doc.norang.ca/org-mode.html

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-capture)

(setq org-deadline-warning-days 10)

(setq org-agenda-files '("~/wrk/org/"))
(setq org-default-notes-file "~/wrk/org/refile.org")

(setq org-todo-keywords
  '((sequence "TODO(t)" "STARTED(s!)" "BLOCKED(b@/!)" "FUTURE(f)" "|" "DONE(d!)" "CANCELLED(c@)")))

(setq org-use-fast-todo-selection t)

(setq org-capture-templates
  '(("t" "todo" entry (file "~/wrk/org/refile.org")
     "* TODO %?\n%U\n%a\n  %i" :clock-in t :clock-resume t)
    ("n" "note" entry (file "~/wrk/org/refile.org")
     "* %? :NOTE:\n%U\n%a\n  %i" :clock-in t :clock-resume t)))

(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))

(setq org-refile-allow-creating-parent-nodes t)
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-completion-use-ido t)
(setq ido-max-directory-size 100000)

(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Unfiled")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-BLOCKED-CANCELLED/!NEXT|STARTED"
                           ((org-agenda-overriding-header "In Progress")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED/!-NEXT-STARTED-BLOCKED-FUTURE"
                           ((org-agenda-overriding-header "Tasks")
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (todo "BLOCKED"
                      ((org-agenda-overriding-header "Waiting and Postponed tasks")
                       (org-agenda-todo-ignore-scheduled t)
                       (org-agenda-todo-ignore-deadlines t)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-todo-ignore-scheduled 'future)
                            (org-agenda-todo-ignore-deadlines 'future)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (todo "FUTURE"
                      ((org-agenda-overriding-header "Future")
                       (org-agenda-todo-ignore-scheduled t)
                       (org-agenda-todo-ignore-deadlines t)))
                )
               nil))))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (let ((has-subtask)
        (subtree-end (save-excursion (org-end-of-subtree t)))
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (forward-line 1)
      (while (and (not has-subtask)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (member (org-get-todo-state) org-todo-keywords-1)
          (setq has-subtask t))))
    (and is-a-task has-subtask)))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (bh/is-project-p)
        nil
      subtree-end)))
