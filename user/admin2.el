
(require 'epresent)

(defun epresent-subtree-in-other-buffer ()
  "Copy current subtree into buffer presentation.org.
Switch to that buffer.
Activate 'org-mode' in that buffer.
Start e-present presentation in that buffer."
  (interactive)
  (org-copy-subtree)
  (switch-to-buffer´´`` "presentation.org")
  (org-mode)
  (epresent-run)
  )

(require 'ox-reveal)

(when (featurep 'yasnippet)
  (message "\n ** loading snippets of iz base-dir %s \n" sclang-orgmode-prelude-base-dir)
  ;; mc: load them once 
  (yas-load-directory (expand-file-name "snippets/" sclang-orgmode-prelude-base-dir))
  ;; mc: keep them availlable on reloads
  (add-to-list 'yas/root-directory (expand-file-name "snippets" sclang-orgmode-prelude-base-dir)) ; mc
  )
