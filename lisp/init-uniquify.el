(jp/install-if-needed 'uniquify)

;; uniquify makes the buffer names unique with path included
(require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; set matlab m-files to load in octave mode
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
