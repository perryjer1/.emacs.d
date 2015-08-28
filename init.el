;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;;                                  Init.el                                 ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-utils)
(require 'init-package)
(require 'init-markdown)
(require 'init-ess)
(require 'init-python)
(require 'init-smex)
(require 'init-magit)
(require 'init-ido)
(require 'init-ibuffer)
(require 'init-windmove)
(require 'init-recentf)
(require 'init-misc)
