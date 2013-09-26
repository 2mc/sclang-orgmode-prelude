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
(define-key global-map "\C-cc" 'org-capture)



(setq org-tag-persistent-alist 
      '(
        (:startgroup) ("@work" . ?w) ("@home" . ?h) (:endgroup )
        ("laptop" . ?l)
        )
      )

(setq org-capture-templates
      '(
        ("i" "income (date prompt)" entry (file+datetree+prompt "~/Dropbox/000WORKFILES/org/ledger.org")
         "* %?\n  :PROPERTIES:\n  :INCOME: %^{INCOME}p\n :DATE: %U\n :ENTRYTYPE: transaction\n :TRANSACTIONTYPE: income\n :END:\n%i\n"
         )
        ("e" "expense (date prompt)" entry (file+datetree+prompt "~/Dropbox/000WORKFILES/org/ledger.org")
         "* %?\n  :PROPERTIES:\n  :EXPENSE: %^{EXPENSE}p\n :DATE: %U\n :ENTRYTYPE: transaction\n :TRANSACTIONTYPE: expense\n :END:\n%i\n"
         )
        ("t" "timesheet (date prompt)" entry (file+datetree+prompt "~/Dropbox/000WORKFILES/org/timesheets.org")
         "* %?\n  :PROPERTIES:\n  :DURATION: %^{DURATION}p\n :DATE: %U\n :END:\n%i\n"
         )
        )
      )

;;; Capture-setup.el ends here

