;; markdown
(require 'markdown-mode)

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(provide 'init-markdown)
