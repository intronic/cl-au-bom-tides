(in-package :au-bom-tides)

;;;lisp-cgi-utils
;; print form with options of:
;; *) select dive-site
;; menu:
;; - list tides for year for site from someday on (default today)
;; - plan trip; choose trip start date, trip length (2 nights/3 days)
;; -

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

(defun reset-button ()
  (html:input '((type . "reset")
		(value . "Reset"))))

(defun button (name value)
  (html:input `((type . "submit")
		(name . ,name)
		(value . ,value))))

(defun form-choose-location ()
  "Prints a selector and the selected results."
  (let* ((btn-do-tides (http:http-query-parameter "do-tides"))
	 (btn-do-trip (http:http-query-parameter "do-trip"))
	 (dive-site (http:http-query-parameter "dive-site"))
	 (site (make-dive-site dive-site))
	 (year (aif (http:http-query-parameter "year") (parse-integer it)))
	 (trip-start (http:http-query-parameter "trip-start"))
	 (trip-days (aif (http:http-query-parameter "trip-days") (parse-integer it))))
    (concatenate 
     'string 
     (html:div
      (html:span
       (html:form-self-post
	(html:fieldset 
	 (html:legend "Dive Sites")
	 (html:form-select "dive-site"
			   (list-dive-sites)
			   (or dive-site (first (list-dive-sites))))
	 (html:label '((for . "year")) "Year")
	 (html:form-select "year"
			   (map-int (curry #'+ (timestamp-year (now))) 2)
			   (or year (timestamp-year (now))))
	 (button "do-tides" "Show Tides")
	 (html:fieldset 
	  (html:legend "Trips")
	  (html:label '((for . "trip-start")) "Intended start day")
	  (html:form-select "trip-start" (coerce +short-day-names+ 'list)
			    (or trip-start "Fri"))
	  (html:label '((for . "trip-days")) "Length in days")
	  (html:form-select "trip-days" (map-int #'1+ 5)
			    (or trip-days 2))
	  (button "do-trip" "Show Optimal Trip Dates"))))))
     (html:div
      (cond 
	(btn-do-tides (format nil "Tides for ~a, ~a:" dive-site year))
	(btn-do-trip (format nil "Optimal dates for ~a day ~a trip to ~a, ~a:" 
			     trip-days trip-start dive-site year))
	(t "")))
     (html:div
      (cond
     	((and btn-do-tides site year)
	 (tides-to-html (read-tides (dive-site-port site) year)
			(compose #'string #'tide-high-low)))
     	((and btn-do-trip site year trip-start trip-days)
     	 (tides-to-html
	  (wolf-rock-expanded (read-tides (dive-site-port site) year)
			      trip-start
			      trip-days)
	  #'(lambda (tide)
	      (if (equal (tide-day tide) trip-start) 
		  "day-trip"
		  "day-other"))))
	(t ""))))))

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
