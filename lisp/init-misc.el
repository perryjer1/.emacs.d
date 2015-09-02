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

;; add highlight-symbol
(jp/install-if-needed 'highlight-symbol)
(require 'highlight-symbol)
(global-set-key (kbd "C-*") 'highlight-symbol-at-point)


;;  https://github.com/ananthakumaran/monky
(jp/install-if-needed 'monky)
(require 'monky)
(setq monky-process-type 'cmdserver)


(provide 'init-misc)
