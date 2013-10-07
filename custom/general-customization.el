;;; emacs-customization-general.el --- Customize some emacs parameters.

;;; Commentary:
;;; Various small customizations.

;;; CODE:

;;; save configuration of emacs buffers at exit

;;; desktop-save-mode creates a problem with the cursor!
;;; Therefore disable, thereby shortening load time. 
;;; For fast access to recent files, use helm-mini command instead (C-c m).
;;; (desktop-save-mode t) ;; disabled

;;; resize main window
(setq default-frame-alist '((width . 100) (height . 65) (menu-bar-lines . 1)))

;;; Set font size to 11 points.
(set-face-attribute 'default nil :height 110)

;;; Set tab width to 4 characters (for code examples in org-mode)
(setq-default tab-width 4) ;; Note: (setq tab-width 4) does not work

;;; Some org-mode customization
(setq org-startup-indented t) ;; auto-indent text in subtrees
(setq org-hide-leading-stars t) ;; hide leading stars in subtree headings

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
  (prelude-mode -1))

(add-hook 'org-mode-hook 'org-turn-off-prelude-mode)

;;; Enable some cool languages in org-babel mode:

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (ruby . t)
   (python . t)
   (perl . t)
   ))

;;; End ORG MODE hooks.

;;; Magit config: Manage git repos from inside emacs
(setq magit-repo-dirs
      '(
        "~/Dropbox/000WORKFILES/org"
        "~/Documents/Dev"
        "~/.emacs.d/personal"
))

;;; Enable snippets with yasnippet

(require 'yasnippet)
(yas-load-directory "~/.emacs.d/personal/snippets/")
(require 'sclang-snippets)
(add-hook 'sclang-mode-hook 'yas/minor-mode-on)

;;; log notes in drawer:
(setq org-log-into-drawer t)

;;; Display habits correctly when rescheduling next date due
(setq org-habit-show-habits-only-for-today nil)

;;; Enable paredit mode
;;; FIXME cannot loaded at this time. Must revise package loading scheme.
;;; (require 'paredit)

;;; Encryption
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

;;; Load epresent package for easy presentations from orgmode
(require 'epresent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Following are disabled for the reasons given below

;;; Start speedbar
;; (speedbar) ;; disabled: interferes with helm

;;; Use a different dark color theme
;;; (load-theme 'solarized-dark t)  ;;; not available in my config yet

;;; emacs-customization-general.el ends here
