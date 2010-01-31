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
