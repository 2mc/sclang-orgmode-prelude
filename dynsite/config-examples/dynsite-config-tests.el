;;; dynsite-config-tests.el --- Check function of config parameters

;;; Code: 

(org-install-site 
 '(
   ;; name of your site: 
   "my-site" 
   ;; path of the folder containing the all org project files:
   "~/SitesSource" 
   ;; path of the folder where the html files will be published:
   "~/Sites" 
   ;; this string will be used as address to upload files to web with rsync:
   "earlabor@earlab.org:public_html/larigot-tests/"
   ))

;;;

org-publish-project-alist

(org-publish "dynsite<SitesSource" t)

(org-publish "dynsite<SitesSource-all" t)

(org-publish "all-all" t)




;;; Check default parameter settings:

org-publish-project-alist

org-export-with-author

org-export-with-email

org-html-doctype

org-html-xml-declaration

org-html-link-up

org-html-link-home

org-html-link-org-files-as-html

org-html-head

org-html-head-extra

org-html-inline-images

org-html-extension

org-html-preamble

org-html-postamble

org-html-head-include-default-style

org-html-head-include-scripts

;;; dynsite-config-tests.el ends here
