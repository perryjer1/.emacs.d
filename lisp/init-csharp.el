;; c# mode
(jp/install-if-needed 'csharp-mode)

(require 'csharp-mode)

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist	'("\\.cs$" . csharp-mode))
(add-hook 'csharp-mode-hook (lambda () (setq default-tab-width 4)))

;; for some reason, csharp-mode messes with reverting buffers,
;; something about flymake-mode interaction--this should fix it:
(jp/install-if-needed 'flymake)

(require 'flymake)

(provide 'init-csharp)
