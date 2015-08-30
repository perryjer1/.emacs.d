;;; Miscellaneous config stuff


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


(provide 'init-misc)
