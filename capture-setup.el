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

(defvar org-folder  "~/Dropbox/000WORKFILES/org/")

(setq monitoring-folder (concat org-folder "monitoring/"))

(setq org-agenda-files (list (concat monitoring-folder "agenda.org")))

(setq org-capture-templates
      '(
        ("s" "scratch (fast note entry)" entry (file (concat org-folder "scratch.org"))
         "* %? %^G\n :PROPERTIES:\n :DATE: %T\n :ENTRYTYPE: note\n :END:\n%i\n"
         )
        ("a" "agenda (date prompt)" entry (file+datetree+prompt (concat monitoring-folder "agenda.org"))
         "* %? %^G\n :PROPERTIES:\n :LOCATION: %^{LOCATION}p\n :DATE: %T\n :ENTRYTYPE: task\n :END:\n%i\n"
         )
        ("i" "income (date prompt)" entry (file+datetree+prompt (concat monitoring-folder "ledger.org"))
         "* %? %^G\n :PROPERTIES:\n :INCOME: %^{INCOME}p\n :DATE: %T\n :ENTRYTYPE: transaction\n :TRANSACTIONTYPE: income\n :END:\n%i\n"
         )
        ("e" "expense (date prompt)" entry (file+datetree+prompt (concat monitoring-folder "ledger.org"))
         "* %? %^G\n :PROPERTIES:\n :EXPENSE: %^{EXPENSE}p\n :DATE: %T\n :ENTRYTYPE: transaction\n :TRANSACTIONTYPE: expense\n :END:\n%i\n"
         )
        ("t" "timesheet (date prompt)" entry (file+datetree+prompt (concat monitoring-folder "timesheets.org"))
         "* %? %^G\n :PROPERTIES:\n :DATE: %T\n :END:\n%i\n"
         )
        )
      )

;;; Create global default tag list available to all files
;;; Note: the org-capture-fill-template function was edited, 
;;; at  'org-tags-completion-function to use these as in 
;;; org-set-tags, calling (org-get-tags-string).
;;; This should be reported to orgmode mailing list. 

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
         (:startgroup) ("diploma" . ?d) ("phd" . ?p) ("class" . ?c) 
         ("meal" . ?m) ("errand" . ?E) ("household" . ?h) (:endgroup)
         (:startgroup) ("research" . ?r) ("web" . ?w) 
         ("reading" . ?b) ("sports" . ?S) (:endgroup)
         (:startgroup) ("emacs" . ?e) ("supercollider" . ?s) 
         ("publication" . ?l) ("artproject" . ?a ) (:endgroup )
         ("laptop" . ?l) ("mt" . nil) ("coffee" . nil) ("toilet" . nil)
         ("sleep" . nil) ("procrastinating" . nil) ("career" . nil) ("avarts" . nil)
         )
       )


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
      (let ((path (concat base "/" f)))
        (when (equal "org" (car (last (split-string f "\\."))))
          (setq paths (cons (cons f path) paths))
          )))
    paths)
  )

(defvar org-refile-persistent-targets nil)

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

(defun org-refile-add-target ()
  "Add to refile targets a file chosen interactively from the subfolders of org source folder."
  (interactive)
  (org-refile-choose-target +1)
)

(defun org-refile-set-target ()
  "Add to refile targets a file chosen interactively from the subfolders of org source folder."
  (interactive)
  (org-refile-choose-target -1)
  )

(defun org-refile-target-key-hook ()
  "Add refile target choice keyboard commands."
  (local-set-key (kbd "C-c T") 'org-refile-add-target)
  (local-set-key (kbd "C-c t") 'org-refile-set-target)
;; and a utility for setting the duration tag
  (local-set-key (kbd "C-c d") 'org-set-duration-property)
  )

(add-hook 'org-mode-hook 'org-refile-target-key-hook)

(defun org-set-duration-property (duration)
  "Set duration property of current entry. Prompt for duration time"
  (interactive "sDuration: ")
  (org-set-property "DURATION" duration)
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

