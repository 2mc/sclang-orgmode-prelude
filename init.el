;;; init.el --- Organized config at emacs startup inside prelude's "personal".

;;; Commentary:
;;; Customize init procedure so that files are neat and tidy:
;;; Packages are in folder "packages".
;;; Single init files are in folder "custom-elisp".
;;; Make packages from the packages folder available.
;;; Then evaluate all files that are in folder "custom-elisp."
;;; Also babel-load file user/<username>.org if present,
;;; where <username> is the string returned by (user-login-name)
;;; <username> can also be obtained from the shell with:
;;; echo $USER.

;;; For a more extensive customization scheme, see Kieran Healy's
;;; "Emacs Starter Kit for the Social Sciences":
;;; http://kieranhealy.org/resources/emacs-starter-kit.html
;;; File starter-kit.org contains advice for customizing your setup
;;; when using startup-schemes like Healy's and the present one.

;;; Code:

;; add orgmode's own package-archive
;(require 'package)
(unless (package-installed-p 'org-plus-contrib)
  (message "*mc: adding org package-archive ++ re-initialize packages")
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize)
  (message "refreshing org package database...")
  (package-refresh-contents)
  (message "... org package database refreshing done!")
)

;; download some additional packages if not present
;; Note IZ: Consider moving this to user/<username>.org (Sun Nov  3 2013)
(message "*mc: require package org-plus-contrib")
(prelude-require-package 'org-plus-contrib)
(message "*mc: require package yasnippet")
(prelude-require-package 'yasnippet)

(defvar sclang-orgmode-prelude-base-dir 
  (file-name-directory (or load-file-name (buffer-file-name)))
  "base path for other stuff. 
  Note IZ Sun Nov 3 2013: Should be removed from here.
  Individual files can add their own variables either locally 
  or globally using (file-name-directory (buffer-file-name)).
  "
)

(let* (
       (base (file-name-directory (or load-file-name (buffer-file-name))))
       (default-directory (concat base "packages"))
       (user-custom-org-file (concat base (user-login-name) ".org"))
       ;; .org file must be in subdir to avoid conflict with auto-loaded .el
       ;; files by prelude.
       (user-custom-org-file (concat base "user/" (user-login-name) ".org")))
  (setq sclang-orgmode-prelude-base-dir base)
  (normal-top-level-add-subdirs-to-load-path)
  ;; Load plain emacs lisp files from folder "custom".
  (dolist (path (file-expand-wildcards (concat base "custom-elisp/*.el")))
    (load-file path))
  ;; Load user-specific file, if present.
  (if (file-exists-p user-custom-org-file)
      (org-babel-load-file user-custom-org-file))
  )

;;; init.el ends here.
