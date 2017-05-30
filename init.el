;;; init.el -- starting point for user initialization


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)
;; I call package-initialize after I set the `package-archives'
;; variable.

(require 'org)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
