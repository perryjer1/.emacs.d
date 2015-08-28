;; (require 'python-mode)
;; (autoload 'python-mode "python mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; (setq py-shell-name "ipython")

;; (setq
;; ; python-shell-interpreter "C:\\Anaconda\\python.exe"
;;  python-shell-interpreter "C:\\Anaconda\\Scripts\\ipython.exe"
;; ; python-shell-interpreter-args
;; ;   "-i C:\\Anaconda\\Scripts\\ipython-script.py"
;; ; python-shell-interpreter "ipython"
;; ; python-shell-interpreter-args ""
;;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;  python-shell-completion-setup-code
;;    "from IPython.core.completerlib import module_completion"
;;  python-shell-completion-module-string-code
;;    "';'.join(module_completion('''%s'''))\n"
;;  python-shell-completion-string-code
;;    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(setq python-shell-interpreter "ipython")


(provide 'init-python)
