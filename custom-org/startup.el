
(load "dired-x")

(eval-after-load "dired"
'(progn
   (define-key dired-mode-map "F" 'my-dired-find-file)
   (defun my-dired-find-file (&optional arg)
     "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
     (interactive "P")
     (let* ((fn-list (dired-get-marked-files nil arg)))
       (mapc 'find-file fn-list)))))

(require 'calfw)

(require 'paredit)

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

(require 'htmlize)
(require 'o-blog)
