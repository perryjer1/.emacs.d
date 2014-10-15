;;;
;;; .emacs.home.el
;;;

(defvar my-location 'home-linux "Where I am.")

(load "~/.emacs.d/general-settings.el")
(load "~/.emacs.d/my-packages.el")

;(set-frame-size (selected-frame) 195 50)
(desktop-save-mode t)
