;;;
;;; .emacs.work.el
;;;

(defvar my-location 'work "Where I am.")

(load "~/.emacs.d/general-settings.el")
(load "~/.emacs.d/my-packages.el")
(load "~/.emacs.d/rcm-settings.el")

;; at work, i use gfind for windows
(setq find-program "gfind")

(set-frame-size (selected-frame) 220 65)
(desktop-save-mode t)
