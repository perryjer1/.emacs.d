;; recentf
;; http://www.emacswiki.org/emacs/RecentFiles
(require 'recentf)

(recentf-mode t)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(provide 'init-recentf)
