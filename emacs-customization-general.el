;;; emacs-customization-general.el --- Customize some emacs parameters.

;;; Commentary:
;;; Various small customizations.

;;; CODE:

;;; save configuration of emacs buffers at exit
(desktop-save-mode t)

;;; resize main window
(setq default-frame-alist '((width . 210) (height . 65) (menu-bar-lines . 1)))

;;; Set font size to 10 points.
(set-face-attribute 'default nil :height 100)

;;; Enable usage of arrow-cursor keys: It is needed for orgmode.
(setq prelude-guru nil)

;;; Start speedbar
(speedbar)

;;; emacs-customization-general.el ends here
