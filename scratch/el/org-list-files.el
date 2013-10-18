;;; Developing a way to manage a bibliography of pdf files stored in 
;;; nested folders under a single folder.
;;; First, build an Org mode file reflecting the hierarchy of subfolders
;;; and listing all pdf files in them. 
;;; More details are in the underway.org file in my work notes folder. 

(require 'paredit)

;;; This starting point to keep:
(require 'find-lisp)

(defvar org-list-files-folders ())

(defun org-list-files (root &optional switches) 
  "Insert folders/files contained in root path, as orgmode tree."
  (interactive (dired-read-dir-and-switches ""))
  (setq org-list-files-folders ())
  (let* 
      ((files 
        (mapcar (lambda (path)
                  (list (file-name-directory path)
                        (file-name-nondirectory path)
                        path))
                (sort
                 (mapcar (lambda (string) (replace-regexp-in-string root "/" string))
                         (find-lisp-find-files root "\.pdf"))
                 'string<))))
    (dolist (file-entry (sort files (lambda (a b) (string< (car a) (car b)))))
      (apply 'org-make-file-entry file-entry))))



(defun org-make-file-entry (path filename fullpath)
  (let (
        (node-prefix "\n")
        (folder-check org-list-files-folders)
        (folders (cdr (split-string path "/")))
        (folderpath "/")
        )
    (dolist (folder folders)
      (setq node-prefix (concat node-prefix "*"))
      (setq folderpath (concat folderpath folder "/"))
      (if (equal folder (car folder-check))
          (setq folder-check (cdr folder-check))
        (progn
          (setq folder-check ())
          (setq org-list-files-folders folders)
          (unless (equal folder "")
            (org-list-files-insert-folder-node node-prefix folder folderpath)))
        ))
    (org-list-files-insert-file-node node-prefix path filename fullpath)))


(defun org-list-files-insert-folder-node (prefix folder folderpath)
  (insert (concat prefix " " folder))
  (insert (format "\n\t:PROPERTIES:\n\t:PATH: %s\n\t:END:" folderpath)))

(defun org-list-files-insert-file-node (prefix path filename fullpath)
  (insert (concat prefix " " filename))
  (insert (format "\n\t:PROPERTIES:\n\t:PATH: %s\n\t:TYPE: FILE\n\t" fullpath))
  (insert (format "\n\t:FILENAME: %s\n\t:END:" filename))
)

(defun org-make-files-table ()
  "Create table listing files collected in org buffer by org-list-files."
  (interactive)
  (let ((entries ()))
    (org-map-entries 
     (lambda ()
       (if (equal "FILE" (org-entry-get (point) "TYPE"))
           (setq 
            entries 
            (cons 
              (list 
               (org-entry-get (point) "FILENAME")
               (org-entry-get (point) "PATH"))
              entries))
         ))
     )
    (insert "|-|-|-|-|\n")
    (insert "| ! | filename | link | stdname |\n")
    (insert "|-|-|-|-|\n")
    (dolist (entry entries)
       (insert (format "| | %s | [[%s][link]] | |\n" (car entry) (cadr entry)))
                             )

    (insert "|-|-|-|-|")))
