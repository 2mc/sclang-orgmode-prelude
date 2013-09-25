
;; The following defines the site for producing the html documentation of DynSite

(org-install-site 
     '("dynsite"
        "~/Dropbox/orgshared/config/dynsite-org" 
 ;; path of the folder where the html files will be published:
        "~/Dropbox/orgshared/config/dynsite-html" 
 ;; this string will be used as address to upload files to web with rsync:
        "earlabor@earlab.org:public_html/larigot-tests/dynsite"))