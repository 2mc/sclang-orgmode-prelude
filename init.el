;;; init.el --- Organized config at emacs startup inside prelude's "personal".

;;; Commentary:
;;; Customize init procedure so that files are neat and tidy:
;;; Packages are in folder "packages".
;;; Single init files are in folder "custom".
;;; Make packages from the packages folder available.
;;; Then evaluate all files that are in folder "custom."
;;; Also babel-load file <username>.org if present,
;;; where <username> is the string returned by (user-login-name)
;;; <username> can also be asked through the shell with:
;;; echo $USER.

;;; For a more extensive customization scheme, see Kieran Healy's
;;; "Emacs Starter Kit for the Social Sciences":
;;; http://kieranhealy.org/resources/emacs-starter-kit.html
;;; File starter-kit.org contains advice for customizing your setup
;;; when using startup-schemes like Healy's and the present one.

;;; Code:

(let* (
       (base (file-name-directory (or load-file-name (buffer-file-name))))
       (default-directory (concat base "packages"))
       (user-custom-org-file (concat base (user-login-name) ".org"))
       )
  (normal-top-level-add-subdirs-to-load-path)
  ;; Load plain emacs lisp files from folder "custom".
  (dolist (path (file-expand-wildcards (concat base "custom-elisp/*.el")))
    (load-file path))
  ;; Also load code in org mode files from folder "custom".
  (dolist (path (file-expand-wildcards (concat base "custom-org/*.org")))
    (org-babel-load-file path))
  (if (file-exists-p user-custom-org-file)
      (org-babel-load-file user-custom-org-file))
  )

;;; init.el ends here.
