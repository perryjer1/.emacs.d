(require 'package)


(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)


(defvar jp/packages-refreshed? nil
  "For use in `jp/install-if-needed` to only refresh package contents once.")


(defun jp/install-if-needed (pkg)
  "Installs a package if it isn't installed already."
  (unless (or (assoc pkg package-archive-contents) jp/packages-refreshed?)
    (package-refresh-contents)
    (setq jp/packages-refreshed? t))
  (if (package-installed-p pkg)
      t
    (package-install pkg)))


(provide 'init-package)
