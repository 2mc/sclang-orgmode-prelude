;; ~/.emacs

;; ============================ 1 ===========================
;; The emacs.el file is the master setup file shared via dropbox
;; between earlab monks. It is the hub of the setup: One and only control center. 
(load-file "~/Dropbox/orgshared/config/emacs.el")
;; Here I temporarily use an experimental alternative: 
;; (load-file "~/Dropbox/orgshared/config/emacs-experimental.el")

;; ============================ 2 ===========================
;; THE REST OF THIS FILE IS DISREGARDED IN MANUAL CONFIGURATION: 
;; The rest of this file is text which is _AUTOMATICALLY ADDED_ 
;; BY THE EMACS FOR MACOS X APPLICATION, WHENEVER ONE 
;; ONE PERFORMS CUSTOMIZATION WITH ITS BUILT IN MECHANISM (using menus of the application)
;; Therefore, leave it alone (disregard ...) 

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ebib-file-associations (quote (("pdf" . "/usr/bin/open") ("ps" . "gv"))))
 '(ebib-file-search-dirs (quote ("~/Dropbox/orgshared/references/pdf/")))
 '(ebib-index-display-fields (quote (author title year)))
 '(ebib-preload-bib-files (quote ("~/Dropbox/orgshared/references/bib/references.bib")))
 '(org-agenda-files (quote ("~/Dropbox/notes/capturefiles/notes.org" "~/Dropbox/notes/capturefiles/ptychiakes.org" "~/Dropbox/notes/capturefiles/projects.org" "~/Dropbox/notes/capturefiles/tasks.org" "~/Dropbox/notes/capturefiles/classes.org" "~/Dropbox/notes/capturefiles/phd.org" "~/Dropbox/notes/capturefiles/ambiant.org" "~/Dropbox/notes/capturefiles/log.org" "~/Dropbox/notes/capturefiles/ideas.org" "~/Dropbox/notes/capturefiles/finance.org" "~/tmp/org_tests.org" "~/Dropbox/Persons+Projects/AB-IZ/note_exchange.org")))
 '(org-site-html "/Users/iani/iani.github.com"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
