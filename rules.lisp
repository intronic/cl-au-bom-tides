(in-package :au-bom-tides)

(defun read-tides (path)
  (with-open-file (s path)
    (read s)))

(defun tide-day (tide)
  (third tide))

(defun tide-universal-time (tide)
  (first tide))

(defun tide-time (tide)
  (universal-to-timestamp (tide-universal-time tide)))

(defun tide-high-low (tide)
  (fourth tide))

(defun tide-height (tide)
  (sixth tide))

(defun tide-max (tide)
  (reduce #'max tide :key #'tide-height))

(defun tide-min (tide)
  (reduce #'min tide :key #'tide-height))

(defun tide-hour (tide)
  (timestamp-hour (tide-time tide)))

(defun tide-min-pctile (tide &key (pct 5/100))
  (tide-height (nth (floor (* pct (/ (length tide) 2)))
		    (sort (copy-list tide) #'< :key #'tide-height))))

(defun tide-max-pctile (tide &key (pct 5/100))
  (tide-height (nth (floor (* pct (/ (length tide) 2)))
		    (sort (copy-list tide) #'> :key #'tide-height))))

; friday, high tide not between 8pm-10pm, high not in top 5% of tides,

(defun wolf-rock (tide)
  ;; friday night high tide, between 10pm-midnight (so we can drive down on the
  ;; beach that night, and high tide on sat is before noon) and the high tide is
  ;; not as high as the top 5% of tides.
  (remove-if #'(lambda (x) (not (and (eq :high (tide-high-low x)) 
				     (string-equal "Fri" (tide-day x))
				     (<= 22 (tide-hour x) 23)
				     (< (tide-height x) (tide-max-pctile tide)))))
	     tide))
