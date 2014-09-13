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
    markdown-mode
    yaml-mode
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

;; got this idea from Yusuke Tsutsumi
;;  http://toumorokoshi.github.io/emacs-from-scratch-part-2-package-management.html
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
(add-to-list 'auto-mode-alist '("\\.bat$" . batch-mode))

;; c# mode
(unless (eql my-location 'work-linux)
  (require 'csharp-mode)
  (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
  (add-to-list 'auto-mode-alist	'("\\.cs$" . csharp-mode))
  (add-hook 'csharp-mode-hook (lambda () (setq default-tab-width 4)))
  ;; for some reason, csharp-mode messes with reverting buffers,
  ;; something about flymake-mode interaction--this should fix it:
  (require 'flymake))

;; markdown
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; add highlight-symbol
(require 'highlight-symbol)
(global-set-key (kbd "C-*") 'highlight-symbol-at-point)


;; ESS
(require 'ess-site)

(cond ((eql my-location 'home)
       (setq inferior-R-program-name "/usr/bin/R"))
      ((eql my-location 'work)
       (setq inferior-R-program-name "C:/Program Files/R/R-3.0.0/bin/x64/Rterm.exe"))
      (t nil))

;; don't ask for starting directory
(setq ess-ask-for-ess-directory nil)

;; just use this as the starting directory
(cond ((eql my-location 'work) (setq ess-directory "C:/Work/Misc/R/"))
      ((eql my-location 'work-linux) (setq ess-directory "~"))
      ((eql my-location 'home) (setq ess-directory "~"))
      (t nil))


;; Slime
(require 'slime)

;; (when (i-am-at 'work)
;;   (setenv "LISPBOX_HOME" "C:/Work/Misc/lisp/lispbox-0.7")
;;   (load "lispbox"))
;; to start slime, type M-x slime
