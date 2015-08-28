(require 'magit)

;; git is not on my path at work...
(when (eql my-location 'work)
  (let ((git-exe "C:/Program Files (x86)/Git/bin/git.exe"))
    (setq magit-git-executable git-exe
	  vc-git-program git-exe)))



(provide 'init-magit)
