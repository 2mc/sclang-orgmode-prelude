
;;; Keyboard macros defined during work

(fset 'org-demote-next-node
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([14 M-S-left] 0 "%d")) arg)))

(fset 'increment-r-and-insert
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("r+RriR" 0 "%d")) arg)))

