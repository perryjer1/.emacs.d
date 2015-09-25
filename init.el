;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;;                                  Init.el                                 ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-settings)
(require 'init-package)

;; built-in packages
(require 'init-ibuffer)
(require 'init-ido)
(require 'init-recentf)
(require 'init-windmove)
(require 'init-uniquify)

;; third party packages
(require 'init-exec-path)
(require 'init-markdown)
(require 'init-ess)
(require 'init-python)
(require 'init-magit)
(require 'init-misc)
