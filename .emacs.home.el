;;;
;;; .emacs.home.el
;;;

(defvar my-location 'home "Where I am.")

(load "~/.emacs.d/general-settings.el")
(load "~/.emacs.d/my-packages.el")

(set-frame-size (selected-frame) 185 48)
(desktop-save-mode t)
