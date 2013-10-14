;;; sclang-snippets.el --- SHORTCUTS FOR EVALUATING REGIONS OF SC CODE

;;; COMMENTARY:
;;; Define keyboard shortcuts in sclang-mode for evaluating regions of
;;; code contained between two lines with comments like this //:

;;; CODE:

(defun sclang-execute-current-snippet ()
  "Evaluate region between //: comments in sclang."
  (interactive)
  (save-excursion
    (let (
          (here (point))
          (blockstart (re-search-backward "^//:" nil t))
          (blockend))
      (if (not blockstart) (setq blockstart 0))
      (set-mark blockstart)
      (goto-char here)
      (setq blockend (re-search-forward "^//:" nil t))
      (if (not blockend) (setq blockend (point-max)))
      (goto-char blockend)
      (sclang-eval-region)
      (set-mark nil)
      )
    )
)

(defun sclang-goto-next-snippet ()
  "Go to the next region delimited with //: comment line."
  (interactive)
  (let ((nextsnippet (re-search-forward "^//:" nil t)))
  (if (not nextsnippet) (setq nextsnippet (point-max)))
  (goto-char nextsnippet)
  (next-line)
  )
)

(defun sclang-goto-previous-snippet ()
  "Go to the preceding region delimited with //: comment line."
  (interactive)
  (let ((previoussnippet (re-search-backward "^//:" nil t)))
  (if (not previoussnippet) (setq previoussnippet (point-min)))
  (goto-char previoussnippet))
  (next-line)
)

(defun sclang-execute-previous-snippet ()
  "Go to the previous sclang snippet and evaluate it."
  (interactive)
  (sclang-goto-previous-snippet)
  (sclang-execute-current-snippet)
)

(defun sclang-execute-next-snippet ()
  "Go to the next sclang snippet and evaluate it."
  (interactive)
  (sclang-goto-next-snippet)
  (sclang-execute-current-snippet)
)

(defun sc-snippets ()
  "Define sclang mode keys for snippets."
  (local-set-key (kbd "C-c .") 'sclang-execute-current-snippet)
  (local-set-key (kbd "C-s-x") 'sclang-execute-current-snippet) ;; alternative
  (local-set-key (kbd "C-s-f") 'sclang-goto-next-snippet)
  (local-set-key (kbd "C-s-b") 'sclang-goto-previous-snippet)
  (local-set-key (kbd "C-s-n") 'sclang-execute-next-snippet)
  (local-set-key (kbd "C-s-p") 'sclang-execute-previous-snippet)
)

;; add to sclang-mode-hook
(add-hook 'sclang-mode-hook 'sc-snippets)

;;; sclang-snippets.el ends here
