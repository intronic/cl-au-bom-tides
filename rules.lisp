(in-package :au-bom-tides)


(defun wolf-rock (tide &key (high-water 1.9) above-high-water 
		  (trip-day "Fri") (hour-from 22) (hour-to 23))
  ;; test that for wolf-rock trip:
  ;; high tide is lower than high-water (or above if above-high-water is true)
  ;; on the trip-day between the hours hour-from and hour-to

  ;; optimal trip on friday: (wolf-rock tide)

  ;; sub-optimal due to very high tide:
  ;; (wolf-rock tide :above-high-water t)

  ;; sub-optimal due to tide high 8pm-10pm Friday night:
  ;; (wolf-rock tide :hour-from 20 :hour-to 21)

  (remove-if #'(lambda (x) (not (and (eq :high (tide-high-low x)) 
				     (string-equal trip-day (tide-day x))
				     (<= hour-from (tide-hour x) hour-to)
				     (funcall (if above-high-water #'>= #'<) 
					      (tide-height x) high-water))))
	     tide))

(defun day-num (time)
  "Number of the day this year."
  (- (day-of time) (day-of (timestamp-minimize-part time :month))))

(defun day-num-list (tides)
  (mapcar (compose #'day-num #'tide-time) tides))

(defun expand-day-num-list (day-list days-to-add)
  "Add days-to-add days to the day-list."
  (iter (for d in day-list)
	(appending 
	 (iter (for i from d to (+ d days-to-add)) 
	       (collect i)))))

(defun expand-days (tides filter-fn &optional (days-to-add 2))
  (let ((day-list (expand-day-num-list 
		   (day-num-list (funcall filter-fn tides))
		   days-to-add)))
    (remove-if #'(lambda (x) (not (member x day-list)))
	       tides
	       :key (compose #'day-num #'tide-time))))
  
(defun wolf-rock-expanded (tides days-to-add)
  (expand-days tides #'wolf-rock days-to-add))
