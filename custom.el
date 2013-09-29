(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" default)))
 '(fci-rule-color "#383838")
 '(org-agenda-files (quote ("~/Dropbox/000WORKFILES/org/monitoring/reviews.org" "~/Dropbox/000WORKFILES/org/scratch.org" "~/Dropbox/000WORKFILES/org/monitoring/ledger.org" "~/Dropbox/000WORKFILES/org/monitoring/agenda.org")))
 '(org-capture-templates (quote (("s" "scratch (fast scratch entry - test)" entry (file (concat org-folder "scratch.org")) "* %? %^G
 :PROPERTIES:
 :DATE: %T
 :ENTRYTYPE: scratch
 :END:
%i
") ("n" "note (fast note entry)" entry (file (concat org-folder "notes.org")) "* %? %^G
 :PROPERTIES:
 :DATE: %T
 :ENTRYTYPE: note
 :END:
%i
") ("a" "agenda (date prompt)" entry (file+datetree+prompt (concat monitoring-folder "agenda.org")) "* %? %^G
 :PROPERTIES:
 :LOCATION: %^{LOCATION}p
 :DATE: %T
 :ENTRYTYPE: task
 :END:
%i
") ("i" "income (date prompt)" entry (file+datetree+prompt (concat monitoring-folder "ledger.org")) "* %?
 :PROPERTIES:
 :INCOME: %^{INCOME}p
 :DATE: %T
 :ENTRYTYPE: transaction
 :TRANSACTIONTYPE: income
 :END:
%i
") ("e" "expense (date prompt)" entry (file+datetree+prompt (concat monitoring-folder "ledger.org")) "* %?
 :PROPERTIES:
 :EXPENSE: %^{EXPENSE}p
 :DATE: %T
 :ENTRYTYPE: transaction
 :TRANSACTIONTYPE: expense
 :END:
%i
") ("t" "timesheet (date prompt)" entry (file+datetree+prompt (concat monitoring-folder "timesheets.org")) "* %? %^G
 :PROPERTIES:
 :DATE: %T
 :END:
%i
"))))
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-collector)))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
