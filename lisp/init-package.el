(require 'package)

(defvar required-packages
  '(
    ess
    ;; slime
    flymake
    batch-mode
    csharp-mode
    markdown-mode
    racket-mode
    yaml-mode
    highlight-symbol
    magit
    monky
    smex
    ) "list of packages to install if missing")



(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; got this idea from Yusuke Tsutsumi
;;  http://toumorokoshi.github.io/emacs-from-scratch-part-2-package-management.html
(defun jp/package-installer ()
  "Install needed packages."
  (interactive)
  (let ((refreshed nil))
    (mapcar
     (lambda (p)
       (unless (package-installed-p p)
	 (unless refreshed
	   (package-refresh-contents)
	   (setq refreshed t))
	 (package-install p)))
     required-packages)))

(jp/package-installer)

(provide 'init-package)
