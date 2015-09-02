;; Slime
(jp/install-if-needed 'slime)
(require 'slime)

;; http://common-lisp.net/project/slime/doc/html/Installation.html#Installation
(require 'slime-autoloads)
(setq inferior-lisp-program "C:/sbcl/sbcl.exe")
(add-to-list 'slime-contribs 'slime-fancy)
(add-to-list 'slime-contribs 'slime-repl)

