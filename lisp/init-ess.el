;; ESS
(require 'ess-site)

;; don't ask for starting directory
(setq ess-ask-for-ess-directory nil)

;; just use this as the starting directory
(setq ess-directory "~")

(add-hook 'ess-mode-hook
		  (lambda ()
			(setq ess-default-style 'GNU)))

;; modified from comint-dynamic-list-input-ring
(defun jp/r-history ()
  "Display R history."
  (interactive)
  (if (or (not (ring-p comint-input-ring))
		  (ring-empty-p comint-input-ring))
      (message "No history")
    (let ((history nil)
		  (history-buffer " *R History*")
		  (conf (current-window-configuration)))
      ;; We have to build up a list ourselves from the ring vector.
      (dotimes (index (ring-length comint-input-ring))
		(push (ring-ref comint-input-ring index) history))
	  (with-output-to-temp-buffer history-buffer
		(with-current-buffer standard-output
		  (mapcar (lambda (x) (insert x) (insert "\n")) history))))))



(provide 'init-ess)
