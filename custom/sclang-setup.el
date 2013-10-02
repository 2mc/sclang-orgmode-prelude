;;; sclang-setup.el --- Setup variables for running SuperCollider in Emacs.

;;; Commentary:
;;; Set up variables required by sclang to run SuperCollider in Emacs.

;;; CODE:

;; Directory of SuperCollider application - for starting the application
(defvar scapp-dir "/Applications/SuperCollider/SuperCollider.app")

;;; Directory of SuperCollider support, for quarks, plugins, help etc.
(defvar sc_userAppSupportDir (expand-file-name "~/Library/Application Support/SuperCollider"))

;; Make path of sclang executable available to emacs shell load path
(push (expand-file-name "Contents/Resources" scapp-dir) exec-path)

;; Global keyboard shortcut for starting sclang
(global-set-key (kbd "C-c S") 'sclang-start)

;; Disable switching to default SuperCollider Workspace when recompiling SClang
(setq sclang-show-workspace-on-startup nil)

;;; sclang-setup ends here
