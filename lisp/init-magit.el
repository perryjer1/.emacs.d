(jp/install-if-needed 'magit)

(require 'magit)


(global-set-key (kbd "C-x g") 'magit-status)

(when (eql system-type 'windows-nt)
  (setenv "GIT_ASKPASS" "git-gui--askpass"))

(provide 'init-magit)
