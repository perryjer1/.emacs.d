;;;
;;; .emacs.work.el
;;;

(defvar my-location 'work-linux "Where I am.")

(load "~/.emacs.d/general-settings.el")
(load "~/.emacs.d/my-packages.el")

(desktop-save-mode t)

; screen gets messed up sometimes
(global-set-key (kbd "<f5>") 'redraw-display)
