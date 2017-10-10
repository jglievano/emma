;;; mod-org.el --- Org setup. -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal setup for Org usage.

;;; Code:

(defvar emma-org-dir (expand-file-name "~/word/org/"))
(defvar emma-org-refile (concat emma-org-dir "refile.org"))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook 'turn-on-font-lock)
(setq org-support-shift-select 'always
      org-use-fast-todo-selection t
      org-treat-S-cursor-todo-selection-as-state-change nil
      org-directory emma-org-dir
      org-default-notes-file emma-org-refile
      org-agenda-dim-blocked-tasks nil
      org-agenda-compact-blocks t)

(setq org-agenda-files '("~/work/org"
                         "~/work/org/google/mobile-ninjas"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "blue" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("HOLD" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)
        ("MEETING" :foreground "forest green" :weight bold)
        ("PHONE" :foreground "forest green" :weight bold)))

(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t))
        ("HOLD" ("WAITING") ("HOLD" . t))
        (done ("WAITING") ("HOLD"))
        ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
        ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
        ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

(setq org-capture-templates
      '(("t" "Todo" entry (file emma-org-refile)
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("r" "Respond" entry (file emma-org-refile)
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
        ("n" "Note" entry (file emma-org-refile)
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("j" "Journal" entry (file+datetree emma-org-refile)
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ("w" "org-protocol" entry (file emma-org-refile)
         "* TODO Review %c\n%U\n" :immediate-finish t)
        ("m" "Meeting" entry (file emma-org-refile)
         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
        ("p" "Phone call" entry (file emma-org-refile)
         "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
        ("h" "Habit" entry (file emma-org-refile)
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

(setq org-agenda-custom-commands
      '(("N" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels t)))
        ("h" "Habits" tags-todo "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-strategy
           '(todo-state-down effort-up category-keep))))
        (" " "Agenda"
         ((agenda "" nil)
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-CANCELED/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-functin 'em/skip-non-stuck-projecs)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-HOLD-CANCELED/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function 'em/skip-non-projects)
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-CANCELED/!NEXT"
                     ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                            (if em/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'em/skip-projects-and-habits-and-single-tasks)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled em/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines em/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date em/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
          (tags-todo "-REFILE-CANCELED-WAITING-HOLD/!"
                     ((org-agenda-overriding-header (concat "Project Subtasks"
                                                            (if em/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'em/skip-non-project-tasks)
                      (org-agenda-todo-ignore-scheduled em/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines em/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date em/hide-scheduled/and-waiting-next-tasks)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-REFILE-CANCELED-WAITING-HOLD/!"
                     ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                            (if em/hide-scheduled-and-waiting-next-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'em/skip-project-tasks)
                      (org-agenda-todo-ignore-scheduled 'em/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines 'em/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-with-date 'em/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-CANCELED+WAITING|HOLD/!"
                     ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                            (if em/hide-scheduled-and-waiting-tasks
                                                                ""
                                                              " (including WAITING and SCHEDULED tasks)")))
                      (org-agenda-skip-function 'em/skip-non-tasks)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-ignore-scheduled em/hide-scheduled-and-waiting-next-tasks)
                      (org-agenda-todo-ignore-deadlines em/hide-scheduled-and-waiting-next-tasks)))
          (tags "-REFILE/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function 'em/skip-non-archivable-tasks)
                 (org-tags-match-list-sublevels nil))))
         nil)))

(provide 'mod-org)
;;; mod-org.el ends here
