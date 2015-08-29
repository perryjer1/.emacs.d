;;; Miscellaneous config stuff

;; uniquify makes the buffer names unique with path included
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; set matlab m-files to load in octave mode
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))



;; batch mode
(jp/install-if-needed 'batch-mode)

(require 'batch-mode)
(autoload 'batch-mode "batch-mode" "Major mode for editing bat files." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . batch-mode))

;; yaml
(jp/install-if-needed 'yaml-mode)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; racket
(jp/install-if-needed 'racket-mode)

(require 'racket-mode)
(add-to-list 'auto-mode-alist '("\\.rkt$" . racket-mode))

;; ;; add highlight-symbol
;; (jp/install-if-needed 'highlight-symbol)
;; (require 'highlight-symbol)
;; (global-set-key (kbd "C-*") 'highlight-symbol-at-point)


;; ;; Slime
;; (jp/install-if-needed 'slime)
;; (require 'slime)

;; ;; http://common-lisp.net/project/slime/doc/html/Installation.html#Installation
;; (require 'slime-autoloads)
;; (setq inferior-lisp-program "C:/sbcl/sbcl.exe")
;; (add-to-list 'slime-contribs 'slime-fancy)
;; (add-to-list 'slime-contribs 'slime-repl)


;;  https://github.com/ananthakumaran/monky
(jp/install-if-needed 'monky)
(require 'monky)
(setq monky-process-type 'cmdserver)


(load-theme 'deeper-blue)

;; make Emacs a server
;; some bug (related to git?) is messing up server-start
;;   http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
;; i changed the owner of ~/.emacs.d/server as suggested in answer.
(unless (server-running-p)
  (server-start))


(desktop-save-mode 1)


;; ;; screen gets messed up sometimes
;; (global-set-key (kbd "<f5>") 'redraw-display)

;; ;; at work, i use gfind for windows
;; (setq find-program "gfind")

(provide 'init-misc)
