;;;
;;; my-packages.el
;;;


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


;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; racket
(require 'racket-mode)
(add-to-list 'auto-mode-alist '("\\.rkt$" . racket-mode))
;; TODO this was for mac at home, should be fixed when i get PATH setup correctly
;; (setq racket-racket-program "/usr/local/bin/racket")

;; ;; add highlight-symbol
;; (require 'highlight-symbol)
;; (global-set-key (kbd "C-*") 'highlight-symbol-at-point)


;; ;; Slime
;; (require 'slime)

;; ;; http://common-lisp.net/project/slime/doc/html/Installation.html#Installation
;; (require 'slime-autoloads)
;; (setq inferior-lisp-program "C:/sbcl/sbcl.exe")
;; (add-to-list 'slime-contribs 'slime-fancy)
;; (add-to-list 'slime-contribs 'slime-repl)



;;  https://github.com/ananthakumaran/monky
(require 'monky)
(setq monky-process-type 'cmdserver)


