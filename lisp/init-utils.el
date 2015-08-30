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
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; make ediff split vertically by default (which emacs calls horizontal)
(setq ediff-split-window-function 'split-window-horizontally
      ediff-merge-split-window-function 'split-window-horizontally)

;; enable commands
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; i don't like typing yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; turn off the toolbar
(tool-bar-mode -1)

;; turn on paren matching
(show-paren-mode 1)

;; default to regex search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; I often type C-x f but not because I want to set the fill
(global-set-key (kbd "C-x f") 'find-file)

(global-set-key (kbd "C-c t") 'toggle-truncate-lines)

(global-set-key (kbd "<f8>") 'window-configuration-to-register)
(global-set-key (kbd "<f9>") 'jump-to-register)

(defun jp/revert-buffer ()
  "Revert buffer."
  (interactive)
  (revert-buffer nil t)
  (message "Reverted buffer '%s'" (buffer-name)))

(global-set-key (kbd "C-c r") 'jp/revert-buffer)
(global-set-key (kbd "C-c R") 'auto-revert-mode)
(global-set-key (kbd "C-c T") 'auto-revert-tail-mode)

(defun jp/dired-home ()
  "Dired my home directory."
  (interactive)
  (dired "~"))

(global-set-key (kbd "C-c j") 'jp/dired-home)

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

(global-set-key (kbd "C-c p") 'jp/copy-filename)

;; switch back and forth quickly
(defun jp/switch-other-buffer ()
  "Switch to other buffer without prompting."
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key (kbd "C-c b") 'jp/switch-other-buffer)


;; use hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)


;; on a mac, i like command to be meta
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

(provide 'init-utils)
