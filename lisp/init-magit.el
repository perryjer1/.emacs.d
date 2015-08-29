(jp/install-if-needed 'magit)

(require 'magit)

;; git is not on my path at work...
(when (eql system-type 'windows-nt)
  (let ((git-exe "C:/Program Files (x86)/Git/bin/git.exe"))
    (setq magit-git-executable git-exe
	  vc-git-program git-exe)))


(global-set-key (kbd "M-<f12>") 'magit-status)


(provide 'init-magit)
