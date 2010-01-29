(in-package :au-bom-tides)

;;;; http://www.bom.gov.au/cgi-bin/oceanography/tides/tide_predications.cgi?location=qld_59980&Submit.x=63&Submit.y=4&tide_hiddenField=Queensland&years=2010&months=Jan&dates=31

(defvar *standard-ports* 
  '(("nsw_60130" "Yamba")
    ("qld_59300" "Abbot Point") 
    ("qld_58230" "Booby Island") 
    ("qld_59320" "Bowen") 
    ("qld_59980" "Brisbane Bar") 
    ("qld_59450" "Bugatti Reef") 
    ("qld_59820" "Bundaberg" "(Burnett Heads)") 
    ("qld_59060" "Cairns") 
    ("qld_59750" "Gladstone") 
    ("qld_60050" "Gold Coast Seaway") 
    ("qld_58200" "Goods Island") 
    ("qld_58180" "Hammond Island" "(Turtle Head)") 
    ("qld_5818a" "Hammond Rock Streams") 
    ("qld_59511" "Hay Point") 
    ("qld_58140" "Ince Point") 
    ("qld_63580" "Karumba") 
    ("qld_58865" "Leggatt Island") 
    ("qld_58890" "Lizard Island") 
    ("qld_59200" "Lucinda" "(Offshore)") 
    ("qld_59510" "Mackay Outer Harbour") 
    ("qld_59950" "Mooloolaba ") 
    ("qld_63540" "Mornington Island ") 
    ("qld_59140" "Mourilyan Harbour") 
    ("qld_59940" "Noosa Head") 
    ("qld_59690" "Port Alma") 
    ("qld_59040" "Port Douglas") 
    ("qld_59410" "Shute Harbour") 
    ("qld_58170" "Thursday Island") 
    ("qld_59250" "Townsville") 
    ("qld_58100" "Twin Island") 
    ("qld_59850" "Urangan")
    ("qld_59840" "Waddy Point" "(Fraser Island)") 
    ("qld_63620" "Weipa" "(Humbug Point)")))

(defun standard-port (name)
  (rassoc name *standard-ports* 
	  :test (lambda (name port) 
		  (string-equal name (first port)))))

(defun standard-port-names ()
  (mapcar #'cdr *standard-ports*))

(defun standard-port-state (port)
  (cond 
    ((string= "qld" (first port) :end2 3)
     "Queensland")
    ((string= "nsw" (first port) :end2 3)
     "New+South+Wales")))

(defun parse-decimal (str)
  (let* ((parts (split-sequence #\. str))
         (a (car parts))
         (b (cadr parts))
         (a1 (parse-integer a))
         (b1 (or (parse-integer (or b "0") :junk-allowed t) 0)))
    (+ a1 (/ b1 (expt 10 (length b))))))

(defun rec-find-if (predicate tree)
  (if (null tree)
      nil
      (or (funcall predicate tree)
	  (if (consp tree)
	      (or (rec-find-if predicate (first tree))
		  (rec-find-if predicate (rest tree)))))))

(defun node-equal-p (x node &key (test #'eq))
  (if (and (consp node) (funcall test (car node) x))
      node))

(defun find-node (type tree)
  (if (consp tree) 
      (or (node-equal-p type tree)
	  (find-node type (first tree))
	  (find-node type (rest tree)))))

(defun find-tide-location (page)
  (rec-find-if #'(lambda (x) 
		   (if (and (node-equal-p :table x) ; table node
			    (node-equal-p :tbody (third x)) ; with tbody
			    (node-equal-p :tr (third (third x))) ; and row
			    (node-equal-p :td (third (third (third x)))) ; cell
			    (node-equal-p :h4 (fourth (third (third (third x)))))
			    (string= (third (fourth (third (third (third x)))))
				     "TIDE PREDICTIONS FOR " 
				     :end1 (length "TIDE PREDICTIONS FOR ")))
		       (fourth (third (third (third x))))))
	       page))

(defun find-tide-table (page)
  (rec-find-if #'(lambda (x) 
		   (if (and (node-equal-p :table x) ; table node
			    (eql 3 (length x))
			    (node-equal-p :tbody (third x)) ; with tbody
			    (>= (length (third x)) (+ 2 5)) ; 2 header & 5+ data rows
					; (fourth (third x)) is second header row 
					; with "Time" & "Ht" labels 
					; and expect 14 columns in the row
			    (= (length (fourth (third x))) (+ 2 14))
			    (equal (third (third (fourth (third x))))
				   "Time")
			    (equal (third (fourth (fourth (third x))))
				   "Ht"))
		       x))
	       page))

(defun date-matches (header-row query-date)
  "Ensure the date in the header row matches the date in the query."
  (equal (parse-integer (third (third (third header-row)))
			:start 3 :junk-allowed t)
	 query-date))

(defun validate-tide-table (page date)
  (let ((table (find-tide-table page)))
    (unless (date-matches (tide-table-tbody table) date)
      (error "Dates in tide table dont match query date ~s~%" 
	     (tide-table-tbody table)))
    table))

(defun tide-table-tbody (table)
  "Get the table tbody from the table"
  (cddr (third table)))

(defun tide-table-data (table)
  "Get the table data rows from the table"
  (cddr (tide-table-tbody table)))

(defun row-columns (row)
  (cddr row))

(defun parse-number-or-null (string)
  (if string (parse-decimal string)))

(defun high-or-low (col)
  (let ((color (second (first (second (third (second col))))))
	(height (third (third (second col)))))
    (cond 
      ((string= color "red") (values (parse-number-or-null 
				      (trim-whitespace-to-null (third height))) 
				     :low))
      ((string= color "blue") (values (parse-number-or-null
				       (trim-whitespace-to-null height))
				      :high)))))

(defun tide-time (col)
  (trim-whitespace-to-null (third (first col))))

(defun trim-whitespace (s)
  (string-trim '(#\Space #\Tab #\Newline #\NO-BREAK_SPACE) s))

(defun trim-whitespace-to-null (s)
  (let ((trim (trim-whitespace s)))
    (if (plusp (length trim))
	trim)))

(defun hhmm->mm (time)
  (parse-integer time :start 2 :end 4))

(defun hhmm->hh (time)
  (parse-integer time :start 0 :end 2))

(defun munge-tide-data (page date)
  "Add the location to the table so users can be sure they are looking at what they expect, in case the codes for locations change."
  (let ((loc (find-tide-location page))
	(tab (validate-tide-table page date)))
    `(:div nil ,loc ,tab)))

(defun html-tide-table-to-stream (port-name year month day stream)
  "Get html table for tides for the location for 7 days from the year, month, and day, and write it to the stream. Port name is the name of the port, for example Brisbane Bar, Yamba, Urangan (see http://www.bom.gov.au/oceanography/tides/MAPS/qld.shtml). Year is a four-digit integer. Month is an integer from 1 to 12. Day is an integer from 1 to 31."
  (let* ((port (standard-port port-name))
	 (port-code (first port))
	 (port-state (standard-port-state port))
	 (month-name (month-name month))
	 (uri (expand-uri-template "http://www.bom.gov.au/cgi-bin/oceanography/tides/tide_predications.cgi?location={port-code}&Submit.x=63&Submit.y=4&tide_hiddenField={port-state}&years={year}&months={month-name}&dates={day}")))
    (serialize-lhtml (munge-tide-data (parse (http-request uri) 
					     (make-lhtml-builder)) day)
		     (make-character-stream-sink stream)))) 

(defun parse-columns (year month days cols)
  (labels ((parse-column (col year month day)
	     (let* ((time-string (tide-time col))
		    (time (if time-string
			      (encode-universal-time 0 
						     (hhmm->mm time-string)
						     (hhmm->hh time-string)
						     day month year))))
	       (multiple-value-bind (height low-high)
		   (high-or-low col)
		 (list time
		       (if time (encoded-timestring time))
		       (if time (encoded-day time))
		       low-high (if height (coerce height 'single-float)) 
		       height))))
	   (parse-acc (cols year month days prev-day)
	     (if (null cols)
		 nil
		 (let* ((day (first days))
			(next-day-p (< day prev-day))
			(new-month (if next-day-p (1+ (mod month 12)) month))
			(new-year (if (and next-day-p (= 1 new-month)) (1+ year) year)))
		   (cons (parse-column cols new-year new-month day)
			 (parse-acc (cddr cols) new-year new-month (cdr days) day))))))
    (parse-acc cols year month days (first days))))

(let ((year 2010) (month 1) (days '(31 1 2 3 4 5 6)))
  (defun parse-rows (rows)
    (remove-if (complement #'first)
	       (if (null rows)
		   nil
		   (append (parse-columns year month days 
					  (row-columns (car rows)))
			   (parse-rows (cdr rows)))))))

(defun test (path year month day)
  (declare (ignore year month day))
  (let ((table (find-tide-table (parse path (make-lhtml-builder)))))
    (mapcar #'parse-row table)))
 
;; (defun remove-blanks (columns)
;;   (do ((col (
