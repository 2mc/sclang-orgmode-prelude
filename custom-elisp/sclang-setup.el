;;; sclang-setup.el --- Setup variables for running SuperCollider in Emacs.

;;; Commentary:
;;; Set up variables required by sclang to run SuperCollider in Emacs.

;;; CODE:

; ;; Directory of SuperCollider application - for starting the application
; (defvar scapp-dir "/Applications/SuperCollider/SuperCollider.app")

; ;;; Directory of SuperCollider support, for quarks, plugins, help etc.
; (defvar sc_userAppSupportDir (expand-file-name "~/Library/Application Support/SuperCollider"))

; ;; Make path of sclang executable available to emacs shell load path
; (push (expand-file-name "Contents/Resources" scapp-dir) exec-path)

; ;; Load sclang package
; (require 'sclang)

; ;; Global keyboard shortcut for starting sclang
; ;(global-set-key (kbd "C-c S") 'sclang-start) ;mc this redefinition does not work anyway 
; (global-set-key (kbd "C-c W") 'sclang-switch-to-workspace) ;; overrides alt-meta switch command

;; Disable switching to default SuperCollider Workspace when recompiling SClang
(setq sclang-show-workspace-on-startup nil)

;; Save results of sc evaluation in elisp variable for access in emacs
(defvar sclang-return-string  nil
  "The string returned by sclang process after evaluating expressions.")

(defadvice sclang-process-filter (before provide-sclang-eval-results)
  "Pass sc eval return string to elisp by setting sclang-return-string variable."
  (setq sclang-return-string (ad-get-arg 1)))

(ad-activate 'sclang-process-filter)

;;; sclang-setup ends here






