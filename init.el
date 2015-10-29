;;; init.el -- starting point for user initialization

(require 'org)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
