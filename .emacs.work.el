;;;
;;; .emacs.work.el
;;;

(defvar my-location 'work "Where I am.")

(load "~/.emacs.d/general-settings.el")
(load "~/.emacs.d/my-packages.el")

(desktop-save-mode t)

;; at work, i use gfind for windows
(setq find-program "gfind")

