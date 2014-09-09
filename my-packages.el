;;;
;;; my-packages.el
;;;


(defvar required-packages
  '(
    ess
    slime
    flymake
    batch-mode
    csharp-mode
    octave-mod
    highlight-symbol
    magit
    ) "list of packages to install if missing")


;; setup package repos
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)


(defun my-package-installer ()
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

(my-package-installer)

;; batch mode
(require 'batch-mode)
(autoload 'batch-mode "batch-mode" "Major mode for editing bat files." t)
(setq auto-mode-alist
      (append '(("\\.bat$" . batch-mode)) auto-mode-alist))

;; c# mode
(require 'csharp-mode)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(add-hook 'csharp-mode-hook (lambda () (setq default-tab-width 4)))
;; for some reason, csharp-mode messes with reverting buffers,
;; something about flymake-mode interaction--this should fix it:
(require 'flymake)

;; set matlab m-files to load in octave mode
(setq auto-mode-alist
      (append '(("\\.m$" . octave-mode)) auto-mode-alist))


;; add highlight-symbol
(require 'highlight-symbol)
(global-set-key (kbd "C-*") 'highlight-symbol-at-point)


;; ESS

(cond ((i-am-at 'work) (require 'ess-site))
      ((i-am-at 'home) (load "ess-site")))

(setq inferior-R-program-name
      (cond ((i-am-at 'work) "C:/Program Files/R/R-3.0.0/bin/x64/Rterm.exe")
	    ((i-am-at 'home) "/usr/bin/R")))

;; ESS: don't ask for starting directory
(setq ess-ask-for-ess-directory nil)

;; ESS: just use this as the starting directory
(when (i-am-at 'work) (setq ess-directory "C:/Work/Misc/R/"))


;; Slime

;; (when (i-am-at 'work)
;;   (setenv "LISPBOX_HOME" "C:/Work/Misc/lisp/lispbox-0.7")
;;   (load "lispbox"))
;; to start slime, type M-x slime
