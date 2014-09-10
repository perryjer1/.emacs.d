
(defun get-my-location ()
  (let ((my-location system-name))
	(cond ((string= my-location "JEREMIAH2W") 'work)
	      ((string= my-location "Jeremiahs-MacBook-Air.local") 'home)
	      ((string= my-location "jeremiah3w.rotellacapital.com") 'work-linux)
	      (t 'unknown))))

(defun i-am-at (place)
  (eql (get-my-location) place))


(defmacro my-global-set-key (keyseq &rest forms)
  `(global-set-key ,keyseq (lambda ()
			     (interactive)
			     ,@forms)))

(my-global-set-key "\C-ce" (find-file "~/Dropbox/config/.emacs"))
(cond ((i-am-at 'work) (my-global-set-key "\C-cw" (dired "C:/Work")))
      ((i-am-at 'home) (my-global-set-key "\C-cw" (dired "~"))))



(unless (i-am-at 'unknown)
  ;; face for shell mode
    (defface rcm-shell-mode
      `((t (:background "gray40" :foreground "gray90")))
      "Face for shell mode.")

    ;; set face for shell mode
    ;;(add-hook 'shell-mode-hook (lambda () (buffer-face-set "rcm-shell-mode")))

    ;; enable desktop-save-mode
  (desktop-save-mode 1))
