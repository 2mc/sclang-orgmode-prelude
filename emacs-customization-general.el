;;; emacs-customization-general.el --- Customize some emacs parameters.

;;; Commentary:
;;; Various small customizations.

;;; CODE:

;;; save configuration of emacs buffers at exit
(desktop-save-mode t)

;;; resize main window
(setq default-frame-alist '((width . 180) (height . 55) (menu-bar-lines . 1)))

;;; emacs-customization-general.el ends here
