;;; keyboard-bindings.el --- Define my own keyboard bindings.

;;; Commentary:
;;; Define my own keyboard bindings.

;;; Code:

;;; Bind helm-mini to Control-c m
(global-set-key (kbd "C-c m") 'helm-mini)

;;; Provide forward and backward-paragraph for Mac keyboards.
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-[") 'backward-paragraph)

;;; NOTE: See emacs-customization-general.el for turning off prelude mode in org-mode.
;;; This is necessary because of keyboard binding imcompatibilities between these two modes.

;;; keyboard-bindings.el ends here
