(in-package :au-bom-tides)

;; tide format
;;((3475574520 :HIGH 1.63 "Fri" "2010-02-19 23:22 (+10:00)")
;; (3476785620 :LOW 0.23 "Sun" "2010-03-07 23:47 (+10:00)")

(defun make-tide (timestamp high-or-low height)
  (list (timestamp-to-universal timestamp)
	high-or-low height 
	(format-timestring nil timestamp :format '(:short-weekday))
	(format-timestring nil timestamp :format '(:year #\- (:month 2) #\- (:day 2) 
					      #\Space (:hour 2) #\: (:min 2)
					      " (" :gmt-offset #\) ))))

(defun make-path (location year)
  (parse-namestring
   (concatenate 'string 
		"data/"
		(substitute #\- #\Space (string-downcase (trim-whitespace location)))
		"-"
		(if (parse-integer (string year)) (string year))
		".lisp")))
	       

(defun read-tides (location year)
  (with-open-file (s (make-path location year))
    (read s)))

(defun read-tides-path (path)
  (with-open-file (s path)
    (read s)))

(defun tide-day (tide)
  (fourth tide))

(defun tide-universal-time (tide)
  (first tide))

(defun tide-time (tide)
  (universal-to-timestamp (tide-universal-time tide)))

(defun tide-high-low (tide)
  (second tide))

(defun tide-height (tide)
  (third tide))

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

(defun tide-lhtml (tide &optional (col-type :td))
  `(:tr ,(if (symbolp (tide-high-low tide))
	     `((:class ,(format nil "~@(~a~)" (symbol-name (tide-high-low tide))))))
	,@(mapcar 
	   #'(lambda (col) 
	       `(,col-type nil 
			   ,(typecase col
				      (symbol (format nil "~@(~a~)" (symbol-name col)))
				      (string col)
				      (t (format nil "~s" col)))))
	   (rest tide))))

(defun tides-to-lhtml (tides)
  `(:table nil 
	   ,(tide-lhtml '(nil "Tide" "Height" "Day" "Date") :th)
	   ,@(mapcar #'tide-lhtml tides)))
