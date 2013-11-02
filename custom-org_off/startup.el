
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

;;; Some org-mode customization
(setq org-startup-indented t) ;; auto-indent text in subtrees
(setq org-hide-leading-stars t) ;; hide leading stars in subtree headings

;;; log notes in drawer:
(setq org-log-into-drawer t)

;;; Display habits correctly when rescheduling next date due
(setq org-habit-show-habits-only-for-today nil)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

(require 'epresent)

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

(require 'ox-reveal)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (ruby . t)
   (python . t)
   (perl . t)
   ))

(require 'htmlize)
(require 'o-blog)

;;; Load latex package
(require 'ox-latex)

;;; Use xelatex instead of pdflatex, for support of multilingual fonts (Greek etc.)
(setq org-latex-pdf-process (list "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f"))

;;; Add beamer to available latex classes, for slide-presentaton format
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

;;; Add memoir class (experimental)
(add-to-list 'org-latex-classes 
             '("memoir" 
               "\\documentclass[12pt,a4paper,article]{memoir}" 
               ("\\section{%s}" . "\\section*{%s}") 
               ("\\subsection{%s}" . "\\subsection*{%s}") 
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-todo-keyword-faces
      '(
        ("TODO"  . (:foreground "firebrick1" :weight bold))
        ("ACTIVE"  . (:foreground "firebrick3" background: "blue" :weight bold))
        ("INACTIVE"  . (:foreground "grey" :weight bold))
        ("PROJECT"  . (:foreground "steelblue" :weight bold))
        ("DONE"  . (:foreground "forestgreen" :weight bold))
        ("MAYBE"  . (:foreground "dimgrey" :weight bold))
        ("CANCELED"  . shadow)
        ))

;;; resize main window
(setq default-frame-alist '((width . 100) (height . 65) (menu-bar-lines . 1)))

;;; Set font size to 11 points.
(set-face-attribute 'default nil :height 110)

;;; Set tab width to 4 characters (for code examples in org-mode)
(setq-default tab-width 4) ;; Note: (setq tab-width 4) does not work

(setq prelude-guru nil)
(setq prelude-whitespace nil)

(defun org-turn-off-prelude-mode ()
  (prelude-mode -1))

(add-hook 'org-mode-hook 'org-turn-off-prelude-mode)

(setq magit-repo-dirs
      '(
        "~/Dropbox/000WORKFILES/org"
        "~/Documents/Dev"
        "~/.emacs.d/personal"
))

(require 'yasnippet)

(message "************ sclang-orgmode-prelude-base-dir %s" sclang-orgmode-prelude-base-dir)
;(yas-load-directory "~/.emacs.d/personal/snippets/")

(yas-load-directory (expand-file-name "snippets/" sclang-orgmode-prelude-base-dir))

(require 'sclang-snippets)
(add-hook 'sclang-mode-hook 'yas/minor-mode-on)
