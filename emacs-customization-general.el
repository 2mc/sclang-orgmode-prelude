;;; emacs-customization-general.el --- Customize some emacs parameters.

;;; Commentary:
;;; Various small customizations.

;;; CODE:

;;; use color theme with stronger contrast for selected regions and sparse trees
;;; (load-theme 'manoj-dark t)

;;; save configuration of emacs buffers at exit
(desktop-save-mode t)

;;; resize main window
(setq default-frame-alist '((width . 210) (height . 65) (menu-bar-lines . 1)))

;;; Set font size to 10 points.
(set-face-attribute 'default nil :height 100)

;;; Enable usage of arrow-cursor keys: It is needed for orgmode.
(setq prelude-guru nil)

;;; Disable prelude whitespace
(setq prelude-whitespace nil)

;;; ORG MODE: Hooks to undo annoying imcompatibilities with prelude:

;;; Turn off long line coloring in org mode
(add-hook 'org-mode-hook 'whitespace-turn-off)

;;; Wrap lines to fit width of window in org mode
(add-hook 'org-mode-hook 'visual-line-mode)

;;; Turn off prelude mode when in org mode. This is necessary because
;;; prelude mode overwrites some important org mode bindings.
(defun org-turn-off-prelude-mode ()
  (prelude-mode -1)
)

(add-hook 'org-mode-hook 'org-turn-off-prelude-mode)

;;; End ORG MODE hooks.

;;; Following are disabled for the reasons given below

;;; Start speedbar
;; (speedbar) ;; disabled: interferes with helm

;;; Use a different dark color theme
;;; (load-theme 'solarized-dark t)  ;;; not available in my config yet

;;; emacs-customization-general.el ends here
