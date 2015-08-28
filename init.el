;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;;                                  Init.el                                 ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defvar my-location 'home "Where I am.")

(require 'init-utils)
(require 'init-package)
(require 'init-markdown)
(require 'init-ess)
(require 'init-python)
(require 'init-smex)
(require 'init-magit)
(require 'init-ido)
(require 'init-ibuffer)

;; TODO old code, to remove...
(load (expand-file-name "general-settings.el" user-emacs-directory))
(load (expand-file-name "my-packages.el" user-emacs-directory))

(desktop-save-mode 1)
