(in-package :au-bom-tides)

;; ~12.5 hours between high tides
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

(defun bundaberg (tide &key (trip-day "Fri") (hour-from 21) (hour-to 23)
		  high-water above-high-water)
  ;; test that for bundaberg trip:
  ;; high tide on the trip-start-day is between the hours hour-from and hour-to.
  ;; or about 12.5 hours later the next day (default ~9:30am-12:30)
  ;; if a high-tide value is specified, then the high is lower than this number
  ;; (or above this number if above-high-water is true)
  ;; (ie. default = High tide Sat morning if trip leaves Fri)
  (remove-if #'(lambda (x) (not (and (eq :high (tide-high-low x)) 
				     (string-equal trip-day (tide-day x))
				     (<= hour-from (tide-hour x) hour-to)
				     (or (null high-water)
					 (funcall (if above-high-water #'>= #'<) 
						  (tide-height x) high-water)))))
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
  
(defun wolf-rock-expanded (tides trip-day days-to-add)
  (expand-days tides #'(lambda (tides) (wolf-rock tides :trip-day trip-day))
	       days-to-add))

(defun bundaberg-expanded (tides trip-day days-to-add)
  (expand-days tides #'(lambda (tides) (bundaberg tides :trip-day trip-day))
	       days-to-add))

(defun day-filter (tides trip-day days-to-add)
  (expand-days tides #'(lambda (tides)
			 (remove-if (complement (curry #'string-equal trip-day)) 
				    tides :key #'tide-day))
	       days-to-add))

