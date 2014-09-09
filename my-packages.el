(defvar required-packages
  '(
    magit
    batch-mode
    csharp-mode
    ) "list of packages to install if missing")


(mapcar 
 (lambda (p) 
   (unless (package-installed-p p) 
     (package-install p))) 
 required-packages)
