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
	monky
    smex
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
      ((eql my-location 'home-linux)
       (setq inferior-R-program-name "/usr/local/bin/R"))
      ((eql my-location 'work)
       (setq inferior-R-program-name "C:/Program Files/R/R-3.1.2/bin/x64/Rterm.exe"))
      ((eql my-location 'work-linux)
       (setq inferior-R-program-name "/usr/local/bin/R"))
      (t nil))

;; don't ask for starting directory
(setq ess-ask-for-ess-directory nil)

;; just use this as the starting directory
(setq ess-directory "~")

(add-hook 'ess-mode-hook
		  (lambda ()
			(setq ess-default-style 'GNU)))

;; modified from comint-dynamic-list-input-ring
(defun jer/r-history ()
  "Display R history."
  (interactive)
  (if (or (not (ring-p comint-input-ring))
		  (ring-empty-p comint-input-ring))
      (message "No history")
    (let ((history nil)
		  (history-buffer " *R History*")
		  (conf (current-window-configuration)))
      ;; We have to build up a list ourselves from the ring vector.
      (dotimes (index (ring-length comint-input-ring))
		(push (ring-ref comint-input-ring index) history))
	  (with-output-to-temp-buffer history-buffer
		(with-current-buffer standard-output
		  (mapcar (lambda (x) (insert x) (insert "\n")) history))))))


;; Slime
(require 'slime)

;; http://common-lisp.net/project/slime/doc/html/Installation.html#Installation
(require 'slime-autoloads)
(setq inferior-lisp-program "C:/sbcl/sbcl.exe")
(add-to-list 'slime-contribs 'slime-fancy)
(add-to-list 'slime-contribs 'slime-repl)


(require 'magit)
;; git is not on my path at work...
(when (eql my-location 'work)
  (setq magit-git-executable "C:/Program Files (x86)/Git/bin/git.exe"))


;;  https://github.com/ananthakumaran/monky
(require 'monky)
(setq monky-process-type 'cmdserver)


(require 'smex)
(global-set-key (kbd "M-x") 'smex)
