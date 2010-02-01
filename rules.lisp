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

