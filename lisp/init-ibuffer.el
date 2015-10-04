;; Ibuffer
;;  http://www.emacswiki.org/emacs/IbufferMode
(require 'ibuffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; don't prompt every time I close buffers
;; http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html
(setq ibuffer-expert t)

(setq ibuffer-saved-filter-groups
      `(("default"
	 ("Dired" (mode . dired-mode))
	 ("Org" (mode . org-mode))
	 ("Python" (or (mode . python-mode)
		       (name . "^\\*Python\\*$")))
	 ("R" (or (name . "^\\*R\\*$")
		  (mode . ess-mode)
		  (mode . ess-help-mode)))
	 ("Emacs Lisp" (or (mode . emacs-lisp-mode)
			   (filename . ,(expand-file-name "~/.emacs"))))
	 ("Stuff" (or (mode . Man-mode)
		      (mode . woman-mode)
		      (mode . Info-mode)
		      (mode . Help-mode)
		      (mode . help-mode)
		      (name . "^\\*"))))))

(add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))

(provide 'init-ibuffer)
