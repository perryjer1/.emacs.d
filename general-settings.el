;;;
;;; general-settings.el
;;;
;;; Everything in this file should be compatible with the standard
;;; Emacs 24.3 distribution.
;;;


;;; global settings

;; move stuff to trash instead of vaporize
(setq delete-by-moving-to-trash t)

;; don't show the start up screen
(setq inhibit-startup-screen t)

;; word wrap off by default
(setq-default truncate-lines t)

;; show column numbers in mode line
(setq column-number-mode t)

;; C-k kills new line
(setq kill-whole-line t)

;; Don't give file too big warning unless file is > 50MB
(setq large-file-warning-threshold (* 50 1000 1000))

;; emacs will prompt before closing
(setq confirm-kill-emacs 'yes-or-no-p)

;; allow C-u C-<SPC> C-<SPC> to cycle mark ring
(setq set-mark-command-repeat-pop t)

;; backup file stuff
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; enable commands
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; i don't like typing yes and no
(defalias 'yes-or-no-p 'y-or-n-p)


;;; built-in packages

;; uniquify makes the buffer names unique with path included
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; IDO mode
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
;; don't prompt to make a new buffer
(setq ido-create-new-buffer 'always)
(ido-mode t)

;; windmove: C-x o -> Shift+arrow
(require 'windmove)
(windmove-default-keybindings)
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; set matlab m-files to load in octave mode
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

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
		 ("dired" (mode . dired-mode))
		 ("python" (mode . python-mode))
		 ("R" (or (name . "^.*\[Rr\]$")
				  (name . "^\\*R\\*$")
				  (name . "^\\*help\\[R\\].*$")))
		 ("org" (mode . org-mode))
         ("emacs.d" (or (filename . ,(expand-file-name "~/.emacs.d/"))
						(filename . ,(expand-file-name "~/.emacs"))))
		 ("Help" (or (mode . Man-mode)
					 (mode . woman-mode)
					 (mode . Info-mode)
					 (mode . Help-mode)
					 (mode . help-mode)))
		 ("Emacs internal" (or (name . "*Messages*")
							   (name . "*scratch*")
							   (name . "*Completions*")
							   (name . "*Helm log*")
							   (name . "*helm recentf*")
							   (name . "*ESS*")
							   (name . "*Compile-Log*"))))))

(add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))

;; recentf
;; http://www.emacswiki.org/emacs/RecentFiles
(require 'recentf)
(recentf-mode t)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;; functions and key bindings

;; I often type C-x f but not because I want to set the fill
(global-set-key "\C-xf" 'find-file)

(global-set-key "\C-ct" 'toggle-truncate-lines)

(defun jp/revert-buffer ()
  "Revert buffer."
  (interactive)
  (revert-buffer nil t)
  (message "Reverted buffer '%s'" (buffer-name)))

(global-set-key "\C-cr" 'jp/revert-buffer)
(global-set-key "\C-cR" 'auto-revert-mode)
(global-set-key "\C-cT" 'auto-revert-tail-mode)

(defun jp/dired-home ()
  "Dired my home directory."
  (interactive)
  (dired "~"))

(global-set-key "\C-cj" 'jp/dired-home)

;; scrolling by default moves the screen too much for me
(defun jp/scroll-left ()
  "Scrolls the window one third to the left."
  (interactive)
  (scroll-left (/ (window-body-width) 3) t))

(defun jp/scroll-right ()
  "Scrolls the window one third to the right."
  (interactive)
  (scroll-right (/ (window-body-width) 3) t))

(global-set-key (kbd "C-<next>") 'jp/scroll-left)
(global-set-key (kbd "C-<prev>") 'jp/scroll-right)

;; next set of functions is used to copy filename to clipboard
(defun jp/get-filename-buffer ()
  "Gets filename of current buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (when buffer (buffer-file-name buffer))))

(defun jp/get-filename-dired ()
  "Gets filename of current dired line."
  (interactive)
  (dired-get-filename nil t))

(defun jp/copy-filename ()
  "Copy filename of buffer or dired line to clipboard."
  (interactive)
  (let ((file-name
	 (if (derived-mode-p 'dired-mode)
	     (jp/get-filename-dired)
	   (jp/get-filename-buffer))))
    (when file-name
      (kill-new file-name)
      (message "'%s' copied to clipboard." file-name))))

(global-set-key "\C-cp" 'jp/copy-filename)

;; switch back and forth quickly
(defun jp/switch-other-buffer ()
  "Switch to other buffer without prompting."
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key "\C-cb" 'jp/switch-other-buffer)

;; make Emacs a server
;; some bug (related to git?) is messing up server-start
;;   http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
;; i changed the owner of ~/.emacs.d/server as suggested in answer.
(server-start)
