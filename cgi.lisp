(in-package :au-bom-tides)

;;;lisp-cgi-utils
;; print form with options of:
;; *) select dive-site
;; menu:
;; - list tides for year for site from someday on (default today)
;; - plan trip; choose trip start date, trip length (2 nights/3 days)
;; -


;; wolf-rock-expanded (tides days-to-add)
;;   (expand-days tides #'wolf-rock days-to-add))

(defun header (title)
  "Set up the page head"
  (http:http-send-headers)
  (princ (html:html-header title :stylesheet "/css/bom.css")))

(defun tide-html-row (tide &optional class-fn (col-type #'html:td))
  (html:tr (if class-fn (list (cons "class" (funcall class-fn tide))))
	   (apply #'concatenate 'string
		  (mapcar 
		   #'(lambda (col) 
		       (funcall col-type
				(typecase col
				  (symbol (format nil "~@(~a~)" (symbol-name col)))
				  (string col)
				  (t (format nil "~s" col)))))
		   (rest tide)))))

(defun tides-to-html (tides &optional class-fn)
  (html:table
   (tide-html-row '(nil "Tide" "Height" "Day" "Date") nil #'html:th)
   (apply #'concatenate 'string (mapcar (rcurry #'tide-html-row class-fn) tides))))

(defun buttons ()
  (concatenate 'string 
	       (html:input '((type . "reset")
			     (value . "Reset")))
	       (html:input '((type . "submit")
			     (name . "do-tides")
			     (value . "Tides")))
	       (html:input '((type . "submit")
			     (name . "do-trip")
			     (value . "Best Trip Times")))))

(defun form-choose-location ()
  "Prints a selector and the selected results."
  (let ((btn-do-tides (http:http-query-parameter "do-tides"))
	(btn-do-trip (http:http-query-parameter "do-trip"))
	(location (http:http-query-parameter "location"))
	(year (aif (http:http-query-parameter "year") (parse-integer it)))
	(trip-start (http:http-query-parameter "trip-start"))
	(trip-days (aif (http:http-query-parameter "trip-days") (parse-integer it))))
    (html:table
     (html:tr
      (html:td "Select location and year: "))
     (html:tr
      (html:td
       (html:form-self-post
	(html:p
	 (html:form-select "location"
			   (list "Brisbane Bar" "Noosa Head")
			   (or location "Brisbane Bar"))
	 (html:form-select "year"
			   (map-int (curry #'+ (timestamp-year (now))) 2)
			   (or year (timestamp-year (now))))
	 (html:form-select "trip-start" (coerce +short-day-names+ 'list) 
			   (or trip-start "Fri"))
	 (html:form-select "trip-days" (map-int #'1+ 5) 
			   (or trip-days 2)))
	(buttons))))
     (html:tr
      (html:td (cond 
		 (btn-do-trip (format nil "Optimal ~s trip dates:" location))
		 (btn-do-tides (format nil "Tides for ~s:" location))
		 (t (format nil "(~s) (~s)~%" 
			    (http:http-query-parameter "do-tides")
			    (http:http-query-parameter "do-trip"))))))
     (html:tr
      (html:td 
       (cond
	 ((and btn-do-tides location year)
	  (tides-to-html (read-tides location year)
			 (compose #'string #'tide-high-low)))
	 ((and btn-do-trip location year)
	  (tides-to-html (wolf-rock-expanded (read-tides location year)
					     trip-start
					     trip-days)
			 #'(lambda (tide)
			     (concatenate 'string (string (tide-high-low tide))
					  (if (equal (tide-day tide) trip-start) "-day" "")))))))))))
  
#|  To make the sbcl exe core:
sbcl --core ../sbcl.core-for-slime
(require :au-bom-tides)
(save-lisp-and-die "sbcl.core.cgi" :toplevel #'au-bom-tides::cgi-handler :executable t)
|#

(defun cgi-handler ()
  (http:http-init)
  
  (header "Tide Calendar")
  
  (princ (html:body
	  (html:h1 "Tide Calendar")
	  (form-choose-location)))
  
  (princ (html:html-footer)))
