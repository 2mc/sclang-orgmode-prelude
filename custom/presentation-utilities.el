;;; presentation-utilities.el --- Facilitate using e-present in org-mode.

;;; Commentary: 
;;; Copy current subtree into buffer presentation.org.
;;; Switch to that buffer.
;;; Activate org-mode in that buffer.
;;; Start e-present presentation in that buffer.

;;; Code: 

(defun epresent-subtree-in-other-buffer ()
  "Copy current subtree into buffer presentation.org.
Switch to that buffer.
Activate 'org-mode' in that buffer.
Start e-present presentation in that buffer."
  (interactive)
  (org-copy-subtree)
  (switch-to-buffer "presentation.org")
  (org-mode)
  (epresent-run)
  )

(provide 'presentation-utilities)
;;; presentation-utilities.el ends here
