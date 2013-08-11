;;; load_personal_folder_packages.el --- Load packages in personal folder

;;; Commentary:
;;; Loads the packages that are contained in the personal folder
;;; The name of each folder placed here must be identical to the name
;;; of the package file to be loaded, minus the .el ending.
;;; If a subfolder does not contain a file with identical name + .el",
;;; then the subfolder is not treated as a package (it is ignored).
;;; Example: to load the package sclang :
;;; Put the package in subfolder "sclang" of folder personal
;;; The name of the package loading file inside sclang must be sclang.el

;;; CODE:

(let (
      (base "~/.emacs.d/personal")
      (packages ())
      (folder-name)
      )
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f "."))
                 (file-exists-p
                  (format "%s/%s.el"
                          name (first (last (split-string name "/") 1))))
                 )
         (setq packages
              (cons (file-name-nondirectory name) packages))
        (add-to-list 'load-path name))))
  (dolist (packagename packages) (require (intern packagename)))
)

;;; load_personal_folder_packages ends here
