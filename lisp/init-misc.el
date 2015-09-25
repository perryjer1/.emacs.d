;;; Miscellaneous config stuff


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


;; highlight-symbol
(jp/install-if-needed 'highlight-symbol)
(require 'highlight-symbol)
(global-set-key (kbd "C-*") 'highlight-symbol-at-point)


;; ace window: https://github.com/abo-abo/ace-window
(jp/install-if-needed 'ace-window)
(require 'ace-window)
(global-set-key (kbd "M-p") 'ace-window)


;; avy: https://github.com/abo-abo/avy
(jp/install-if-needed 'avy)
(require 'avy)
(global-set-key (kbd "M-g l") 'avy-goto-line)


;; smex
(jp/install-if-needed 'smex)
(require 'smex)
(global-set-key (kbd "M-x") 'smex)


;; monky: https://github.com/ananthakumaran/monky
(jp/install-if-needed 'monky)
(require 'monky)
(setq monky-process-type 'cmdserver)


;; expand region: https://github.com/magnars/expand-region.el
(jp/install-if-needed 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;; browse kill ring: https://github.com/browse-kill-ring/browse-kill-ring
(jp/install-if-needed 'browse-kill-ring)


;; setup file searches depending on what is available
(defvar jp/file-search-command nil)

(cond
 ((executable-find "ag")
  (jp/install-if-needed 'ag)
  (require 'ag)
  (setq jp/file-search-command 'ag))
 ((executable-find "ack")
  (jp/install-if-needed 'full-ack)
  (require 'full-ack)
  (setq jp/file-search-command 'ack))
 ((executable-find "grep")
  (setq jp/file-search-command 'grep))
 (t nil))

(defun jp/do-file-search ()
  (interactive)
  (call-interactively jp/file-search-command))

(global-set-key (kbd "M-<f11>") 'jp/do-file-search)


(provide 'init-misc)
