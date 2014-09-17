;;
;; rcm-settings.el
;;

(defface rcm/d2-order-live `((t (:background "orange1"))) "Face for D2 live orders.")
(defface rcm/d2-order-theo `((t (:background "orange3"))) "Face for D2 theoretical orders.")
(defface rcm/d2-fill-live `((t (:background "PaleGreen1"))) "Face for D2 live fills.")
(defface rcm/d2-fill-theo `((t (:background "PaleGreen3"))) "Face for D2 theoretical fills.")
(defface rcm/d2-price-msg `((t (:background "SkyBlue1"))) "Face for D2 price messages.")
(defface rcm/d2-price-bar-open `((t (:background "SkyBlue3"))) "Face for D2 price bars or open.")
(defface rcm/d2-price-missing `((t (:background "yellow"))) "Face for D2 price missing messages.")
(defface rcm/d2-user-log `((t (:background "gray60"))) "Face for D2 user log messages.")
(defface rcm/d2-error `((t (:weight bold :background "yellow" :foreground "red")))
  "Face for D2 errors.")

(defun rcm/d2-log-file ()
  "Add highlighting for d2 log file."
  (interactive)
  (progn
	(hi-lock-mode nil) ; turn on hi-lock-mode if it's not on yet
	(highlight-lines-matching-regexp "UI:O[NXR][CR]?|" "rcm/d2-order-live")
	(highlight-lines-matching-regexp "UI:O[NXR][CR]?T|" "rcm/d2-order-theo")
	(highlight-lines-matching-regexp "UI:FL|" "rcm/d2-fill-live")
	(highlight-lines-matching-regexp "UI:FLT|" "rcm/d2-fill-theo")
	(highlight-lines-matching-regexp "UI:PB[SER]|" "rcm/d2-price-msg")
	(highlight-lines-matching-regexp "UI:P[BO]|" "rcm/d2-price-bar-open")
	(highlight-lines-matching-regexp "UI:PM|" "rcm/d2-price-missing")
	(highlight-lines-matching-regexp "Designer21.UserLogMessage" "rcm/d2-user-log")
	(highlight-lines-matching-regexp "\\(UI:EXC\\|[Ee][Rr][Rr][Oo][Rr]\\|[Ff][Aa][Tt][Aa][Ll]\\|[Ee][Xx][Cc][Ee][Pp][Tt]\\)" "rcm/d2-error")))

(defvar rcm/staging-log-locations-alist nil
  "List to store root result paths for staging runs.")

(setq rcm/staging-log-locations-alist
  (list
   "//xsd2x01/c$/D2/Results/AS/Surya_AA"
   "//xsd2x03/c$/D2/Results/AS/Surya_AA"
   "//xsd2x01/c$/D2/Results/AS/Surya_EU"
   "//xsd2x03/c$/D2/Results/AS/Surya_EU"
   "//xsd2x01/c$/D2/Results/AS/Surya_NA"
   "//xsd2x03/c$/D2/Results/AS/Surya_NA"
   "//xsd2x01/c$/D2/Results/NEUTRINO/Tau"
   "//xsd2x03/c$/D2/Results/NEUTRINO/Tau"
))

(defun rcm/load-staging-dirs (date)
  "Load staging dirs for `date', using `rcm-staging-log-locations-alist'."
  (mapcar #'dired
	  (mapcar (lambda (x)
		    (concat x "/" (if (stringp date) date (number-to-string date))))
		  rcm-staging-log-locations-alist)))


(defun rcm/load-staging-today ()
  "Load staging dirs for today, using `rcm-staging-log-locations-alist'."
  (interactive)
  (rcm/load-staging-dirs (format-time-string "%Y%m%d")))
