(in-package :au-bom-tides)

; friday, high tide not between 8pm-10pm, high not in top 5% of tides,

(defun wolf-rock (tide &optional (trip-day "Fri"))
  ;; friday night high tide, between 10pm-midnight (so we can drive down on the
  ;; beach that night, and high tide on sat is before noon) and the high tide is
  ;; not as high as the top 5% of tides.
  (remove-if #'(lambda (x) (not (and (eq :high (tide-high-low x)) 
				     (string-equal trip-day (tide-day x))
				     (<= 22 (tide-hour x) 23)
				     (< (tide-height x) (tide-max-pctile tide)))))
	     tide))

(defun wolf-rock-bad-tide-height (tide &optional (trip-day "Fri"))
  ;; sub-optimal due to very high tide
  (remove-if #'(lambda (x) (not (and (eq :high (tide-high-low x)) 
				     (string-equal trip-day (tide-day x))
				     (<= 22 (tide-hour x) 23)
				     (>= (tide-height x) (tide-max-pctile tide)))))
	     tide))

(defun wolf-rock-bad-time-trip-day (tide &optional (trip-day "Fri"))
  ;; sub-optimal due to tide high 8pm-10pm Friday night
  (remove-if #'(lambda (x) (not (and (eq :high (tide-high-low x)) 
				     (string-equal trip-day (tide-day x))
				     (<= 20 (tide-hour x) 21)
				     (< (tide-height x) (tide-max-pctile tide)))))
	     tide))
