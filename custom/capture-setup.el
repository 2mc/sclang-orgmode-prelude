;;; capture-setup.el --- Setup capture.

;;; Commentary:
;;; Setup capture.
;;; New ideas: Capture for expense-income logging:
;;; file: ledger.org
;;; Capture mode: file-datetree
;;; Each entry starts with an inactive timestamp.
;;; Prompt for title of entry appended to timestamp.
;;; Two interactive choices for entries:
;;; i: income.  e: expense.
;;; These are marked with property "INCOME_EXPENSE".
;;; Prompt for value of property: "AMOUNT".  (Should be a number)
;;; Finally prompt for value of property: "TYPE".
;;; Values for TYPE are free.  Typical values: food, rent, phone, salary, household, etc.

;;; CODE:

;;; Define shortcut key for evoking org capture
(define-key global-map "\C-cc" 'org-capture)


;; Note: For the following two functions: Look into file-expand-wildcards.
;; This function will render the next two functions obsolete.
(defun get-subfolders-with-paths (base)
  (let ((paths ()))
    (dolist (f (directory-files base))
      (let ((path (concat base f)))
        (when (and (file-directory-p path)
                   (not (equal f ".."))
                   (not (equal f "."))
                   )
          (setq paths (cons (cons f path) paths))
          )))
    paths)
  )

(defun get-orgfiles-with-paths (base)
  "Get org mode subfiles."
  (let ((paths ()))
    (dolist (f (directory-files base))
      (let ((path (concat base f)))
        (when (equal "org" (car (last (split-string f "\\."))))
          (setq paths (cons (cons f path) paths))
          )))
    paths)
  )

(defvar org-folder  "~/Dropbox/000WORKFILES/org/")
(setq monitoring-folder (concat org-folder "monitoring/"))
(setq agenda-folder (concat org-folder "agenda/"))

(setq org-agenda-files
      (mapcar (lambda (l) (cdr l)) (get-orgfiles-with-paths agenda-folder)))

(setq org-capture-templates
      '(
        ("s" "Very temporary notes (scratch.org)" entry (file (concat org-folder "scratch.org"))
         "* %? %^G\n :PROPERTIES:\n :DATE: %T\n :ENTRYTYPE: scratch\n :END:\n%i\n"
         )
        ("S" "Someday: Ideas and notes to process later (personal/someday.org)" entry (file (concat org-folder "personal/someday.org"))
         "* %? %^G\n :PROPERTIES:\n :DATE: %T\n :ENTRYTYPE: note\n :END:\n%i\n"
         )
        ("c" "Calendar: Appointments, Events, Deadlines (agenda/calendar.org)" entry (file+datetree+prompt (concat agenda-folder "calendar.org"))
         "* %? %^G\n :PROPERTIES:\n :LOCATION: %^{LOCATION}p\n :DATE: %T\n :ENTRYTYPE: task\n :END:\n%i\n"
         )
        ("t" "Tasks: Single tasks to process (agenda/tasks.org)" entry (file (concat agenda-folder "tasks.org"))
         "* TODO %? %^G\n :PROPERTIES:\n :DATE: %T\n :ENTRYTYPE: note\n :END:\n%i\n"
         )
        ("h" "Habits: Repeated tasks monitored graphically (agenda/habits.org)" entry (file (concat agenda-folder "habits.org"))
         "* TODO %? %^G\n SCHEDULED: %t\n :PROPERTIES:\n :STYLE: habit\n :LOGGING: DONE(@/!) lognoterepeat\n :END:\n%i\n")
        ("r" "Repeated: Repeated tasks to monitor (agenda/repeated.org)" entry (file (concat agenda-folder "repeated.org"))
         "* TODO %? %^G\n SCHEDULED: %t\n :PROPERTIES:\n :LOGGING: DONE(@/!) lognoterepeat\n :END:\n%i\n")
        ("i" "Income (monitoring/ledger.org)" entry (file+datetree+prompt (concat monitoring-folder "ledger.org"))
         "* %?\n :PROPERTIES:\n :INCOME: %^{INCOME}p\n :DATE: %T\n :ENTRYTYPE: transaction\n :TRANSACTIONTYPE: income\n :END:\n%i\n"
         )
        ("e" "Expense (monitoring/ledger.org)" entry (file+datetree+prompt (concat monitoring-folder "ledger.org"))
         "* %?\n :PROPERTIES:\n :EXPENSE: %^{EXPENSE}p\n :DATE: %T\n :ENTRYTYPE: transaction\n :TRANSACTIONTYPE: expense\n :END:\n%i\n"
         )
        ("T" "Timesheet (monitoring/timesheets.org)" entry (file+datetree+prompt (concat monitoring-folder "timesheets.org"))
         "* %? %^G\n :PROPERTIES:\n :DATE: %T\n :END:\n%i\n"
         )
        ("R" "Review (monitoring/reviews.org)" entry (file+datetree+prompt (concat monitoring-folder "reviews.org"))
         "* %? %^G\n :PROPERTIES:\n :DATE: %T\n :END:\n%i\n"
         )
        )
      )

;;; Create global default tag list available to all files

;; NOTE: The persistent alist is not used by org-capture.  org-rag-alist is.
;; (setq org-tag-persistent-alist
;;       '(
;;         (:startgroup) ("work" . ?W) ("home" . ?H) (:endgroup )
;;         (:startgroup) ("diploma" . ?d) ("phd" . ?p) ("class" . ?c)
;;         ("meal" . ?m) ("errand" . ?E) ("household" . ?h) (:endgroup)
;;         (:startgroup) ("research" . ?r) ("web" . ?w)
;;         ("reading" . ?b) ("sports" . ?S) (:endgroup)
;;         (:startgroup) ("emacs" . ?e) ("supercollider" . ?s)
;;         ("publication" . ?l) ("artproject" . ?a ) (:endgroup )
;;         ("laptop" . ?l) ("mt" . nil) ("coffee" . nil) ("toilet" . nil)
;;         ("sleep" . nil) ("procrastinating" . nil)
;;         )
;;       )

 (setq org-tag-alist
       '(
         (:startgroup) ("work" . ?W) ("home" . ?H) (:endgroup )
         (:startgroup) ("finance" . ?f) ("diploma" . ?d) ("phd" . ?p) ("class" . ?c)
         ("meal" . ?m) ("errand" . ?E) ("household" . ?h) (:endgroup)
         (:startgroup) ("research" . ?r) ("web" . ?w)
         ("reading" . ?b) ("sports" . ?S) (:endgroup)
         (:startgroup) ("emacs" . ?e) ("supercollider" . ?s)
         ("publication" . ?l) ("artproject" . ?a ) (:endgroup )
         ("laptop" . ?l) ("mt" . nil) ("coffee" . nil) ("toilet" . nil)
         ("sleep" . nil) ("procrastinating" . nil) ("career" . nil) ("avarts" . nil)
         ("food" . nil) ("restaurant" . nil)
         )
       )

(defvar org-refile-persistent-targets nil)

(setq org-refile-targets
      (quote ((nil :maxlevel . 9)
              (org-refile-persistent-targets :maxlevel . 9))))

(defun org-refile-choose-target (add-to-targets)
  "Choose a new refile target from a list of folders and files."
  (let ((paths) (choice) (path-names))
    (setq paths (get-subfolders-with-paths org-folder))
    (setq path-names (mapcar (lambda (l) (car l)) paths))
    (setq choice (org-completing-read "choose folder: " path-names))
    (if (string= choice "") (setq choice (car path-names)))
    (setq choice (cdr (assoc choice paths)))
    (setq paths (get-orgfiles-with-paths choice))
    (setq path-names (mapcar (lambda (l) (car l)) paths))
    (setq choice (org-completing-read "choose file: " path-names))
    (if (string= choice "") (setq choice (car path-names)))
    (if (> add-to-targets 0)
        (add-to-list choice org-refile-persistent-targets)
      (setq org-refile-persistent-targets (list choice)))
    (setq org-refile-targets
          (quote ((nil :maxlevel . 9)
                  (org-refile-persistent-targets :maxlevel . 9)))))
)

(defun org-refile-set-target (&optional choosebuffer)
  "Set the refile target to the file of this buffer.
  If invoked with an argument (C-u) then choose file interactively
  from the subfolders of org source folder."
  (interactive "P")
  (if choosebuffer
      (org-refile-choose-target +1)
    (let ((path (buffer-file-name)))
      (if path
          (setq org-refile-persistent-targets (list path))
        (message "cannot add a buffer with no file name to capture target list")
      ))))


(defun org-refile-add-target (&optional choosebuffer)
  "Add to refile targets the file of this buffer.
  If invoked with an argument (C-u) then choose file interactively
  from the subfolders of org source folder."
  (interactive "P")
  (if choosebuffer
      (org-refile-choose-target +1)
    (let ((path (buffer-file-name)))
      (if path
          (add-to-list path org-refile-persistent-targets)
        (message "cannot add a buffer with no file name to capture target list")
        ))))

(defun org-refile-set-target ()
  "Add to refile targets a file chosen interactively from the subfolders of org source folder."
  (interactive)
  (org-refile-choose-target -1)
  )

(defun org-refile-target-key-hook ()
  "Add refile target choice keyboard commands."
  (local-set-key (kbd "C-c T") 'org-refile-add-target)
  (local-set-key (kbd "C-c t") 'org-refile-set-target)
;; and a utility for setting the DATE tag
  (local-set-key (kbd "C-c d") 'org-set-date-property)
  )

(add-hook 'org-mode-hook 'org-refile-target-key-hook)

(defun org-set-date-property ()
  "Set date property of current entry to now."
  (interactive)
  (org-set-property "DATE" (format-time-string (cdr org-time-stamp-formats) (current-time)))
)

;;; Not strictly capture, but similar, as it gives quick access to group of files:
;;; Open a file contained in the main org folder or its subfolders, using projectile.
(defun projectile-find-org-file ()
  "Open a file from the main org folder using projectile."
  (interactive)
  (progn
    (find-file (concat org-folder "scratch.org"))
    (ido-find-file)
    ))

;;; We want this to be accessible per keyboard command everywhere:
(global-set-key (kbd "C-c O") 'projectile-find-org-file)

;;; Capture-setup.el ends here
