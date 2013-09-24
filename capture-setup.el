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

(setq org-capture-templates
      '(
        ("i" "income" entry (file+datetree "~/Dropbox/notes/capturefiles/ledger.org")
         "* %?\n  :PROPERTIES:\n  :AMOUNT: %^{AMOUNT}p\n :CATEGORY: %^{CATEGORY}p\n :DATE: %U\n :ENTRYTYPE: transaction\n :TRANSACTIONTYPE: income\n :END:\n%i\n")
        ("e" "expense" entry (file+datetree "~/Dropbox/notes/capturefiles/ledger.org")
         "* %?\n  :PROPERTIES:\n  :AMOUNT: %^{AMOUNT}p\n :CATEGORY: %^{CATEGORY}p\n :DATE: %U\n :ENTRYTYPE: transaction\n :TRANSACTIONTYPE: expense\n :END:\n%i\n")
        )
      )

;;; capture-setup.el ends here

