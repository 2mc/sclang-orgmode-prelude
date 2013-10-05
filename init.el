;;; init.el --- Organized config at emacs startup inside prelude's "personal".

;;; Commentary:
;;; Customize init procedure so that files are neat and tidy:
;;; Packages are in folder "packages".
;;; Single init files are in folder "custom".
;;; Make packages from the packages folder available.
;;; Then evaluate all files that are in folder "custom."

;;; Code:

(let* (
       (base "~/.emacs.d/personal/")
       (default-directory (concat base "packages")))
  (normal-top-level-add-subdirs-to-load-path)
  (dolist (path (file-expand-wildcards (concat base "custom/*.el")))
    (load-file path)))

;;; init.el ends here.
