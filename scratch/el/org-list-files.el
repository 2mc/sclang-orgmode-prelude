;;; Developing a way to manage a bibliography of pdf files stored in 
;;; nested folders under a single folder.
;;; First, build an Org mode file reflecting the hierarchy of subfolders
;;; and listing all pdf files in them. 
;;; More details are in the underway.org file in my work notes folder. 

(require 'find-lisp)

(require 'paredit)

;;; This starting point to keep: 
(defun org-list-files () 
  "Later add arguments for root and filetype"
  (interactive)
  (let* 
      ((root (expand-file-name "~/Documents/publications_others/WorkMethods/"))
       (files 
        (mapcar (lambda (path)
                  (list (file-name-directory path)
                        (file-name-nondirectory path)
                        path))
                (sort
                 (mapcar (lambda (string) (replace-regexp-in-string root "/" string))
                         (find-lisp-find-files root "\.pdf"))
                 'string<))))
    (dolist (file (sort files (lambda (a b) (string< (car a) (car b)))))
      (insert (format "\nPATH: %s --FILE: %s"      ;;;;  "\nPATH: %s\nFILE: %s\nFULLPATH: %s" 
                      (car file)
                      (cadr file)
                      (caddr file))))))

;;;;;;; Some tests: 

(file-name-directory "abc")
(file-name-directory "abc.fg")
(file-name-directory "/abc.fg")
(file-name-directory "./abc.fg")
(file-name-directory "/usr/abc.fg")
(file-name-directory "usr/local/lib/abc.fg")

(car '("a" "b" "c"))
(cadr '("a" "b" "c"))
(caddr '("a" "b" "c"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Following FAILED
;;; from find-lisp.el
;;; Descend recursively into directory.  Following derived functions
;;; add actions to do for each folder or file visited.
(defun find-lisp-find-files-doing (directory regexp dirfunc filefunc)
  "Find files in DIRECTORY which match REGEXP."
  (let ((file-predicate      'find-lisp-default-file-predicate)
	(directory-predicate 'find-lisp-default-directory-predicate)
	(find-lisp-regexp regexp))
    (find-lisp-find-files-doing-internal
     directory
     file-predicate
     directory-predicate
     dirfunc
     filefunc
     )))

;; Workhorse function
(defun find-lisp-find-files-doing-internal
  (directory file-predicate directory-predicate dirfunc filefunc)
  "Find files under DIRECTORY which satisfy FILE-PREDICATE.
FILE-PREDICATE is a function which takes two arguments: the file and its
directory.

DIRECTORY-PREDICATE is used to decide whether to descend into directories.
It is a function which takes two arguments, the directory and its parent."
  (setq directory (file-name-as-directory directory))
  (let (results sub-results)
    (dolist (file (directory-files directory nil nil t))
      (let ((fullname (expand-file-name file directory)))
         (when (file-readable-p (expand-file-name file directory))
          ;; If a directory, check it we should descend into it
         (and (file-directory-p fullname)
  ;;            (insert (format "DIRECTORY %s\n" fullname))
               (funcall directory-predicate file directory)
               ;; directory-predicate and directory-p confirm this is directory
               ;; Here we insert the extra processing for directories
               (funcall dirfunc file directory)
               (progn
                 (setq sub-results
                       (find-lisp-find-files-doing-internal
                        fullname
                        file-predicate
                        directory-predicate
                        dirfunc
                        filefunc))
                 (if results
                     (nconc results sub-results)
                   (setq results sub-results))))
          ;; For all files and directories, call the file predicate
          (and
  ;;         (insert (format "FILE %s\n" fullname))
           (funcall file-predicate file directory)
               ;; file-predicate decided it is a file, so process it
               ;; Here we insert the extra processing for files
                        (funcall filefunc file directory)
               (if results
                   (nconc results (list fullname))
                 (setq results (list fullname)))))))
    results))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-insert-folder-node (file directory)
  "Format folder node and insert in buffer."
  (insert (format "\n------- FOLDER %s | %s" directory file)))

(defun org-insert-file-node (file directory)
  "Format folder node and insert in buffer."
  (insert (format "\n..... FILE %s | %s" directory file)))


(replace-regexp-in-string )

(sort '("asdf" "qwet" "bfdss") 'string<)

(string< "a" "b")

(defun org-list-files-failed () 
  "Later add arguments for root and filetype"
  (interactive)
  (let 
      ((root (expand-file-name "~/Documents/publications_others/WorkMethods/")))
    (find-lisp-find-files-doing 
     root 
     "\.pdf" 
     'org-insert-folder-node
     'org-insert-file-node)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar path-nodes nil)

(setq path-nodes (cons "asd" path-nodes))

(member "asdasd" path-nodes)

(remove "asd" path-nodes)

(defun make-file-entry (path)
  "construct Org mode node from path and insert in tree"
  (let ())
  (concat path "\n")
)



(split-string "/Users/iani2/" "/")

(length "")

(file-name-as-directory "/Documents/publications_others/WorkMethods/a.pdf")
