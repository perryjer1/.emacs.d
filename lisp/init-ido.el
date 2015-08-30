;; IDO mode
(require 'ido)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; don't prompt to make a new buffer
(setq ido-create-new-buffer 'always)
(ido-mode t)

(global-set-key (kbd "C-c i") 'ido-mode)

(provide 'init-ido)
