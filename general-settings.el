;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General settings

;; move stuff to trash instead of vaporize
(setq delete-by-moving-to-trash t)

;; don't show the start up screen
(setq inhibit-startup-screen t)

;; word wrap off by default
(setq-default truncate-lines t)

;; C-k kills new line
(setq kill-whole-line t)

;; Don't give file too big warning unless file is > 50MB
(setq large-file-warning-threshold (* 50 1000 1000))

;; emacs will prompt before closing
(setq confirm-kill-emacs 'yes-or-no-p)

;; allow C-u C-<SPC> C-<SPC> to cycle mark ring
(setq set-mark-command-repeat-pop t)

;; backup file stuff
(setq backup-directory-alist `(("." . "~/.emacs.backups")))
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

