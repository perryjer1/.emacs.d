;;;
;;; general-settings.el
;;;
;;; Everything in this file should be compatible with the standard
;;; Emacs 24.3 distribution.
;;;


;;; built-in packages

;; uniquify makes the buffer names unique with path included
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; windmove: C-x o -> Shift+arrow
(require 'windmove)
(windmove-default-keybindings)
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; set matlab m-files to load in octave mode
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; recentf
;; http://www.emacswiki.org/emacs/RecentFiles
(require 'recentf)
(recentf-mode t)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;; functions and key bindings

(load-theme 'deeper-blue)

;; make Emacs a server
;; some bug (related to git?) is messing up server-start
;;   http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
;; i changed the owner of ~/.emacs.d/server as suggested in answer.
(server-start)
