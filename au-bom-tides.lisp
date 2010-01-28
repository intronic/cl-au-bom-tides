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

(defun rec-find-if (predicate tree)
  (if (null tree)
      nil
      (or (funcall predicate tree)
	  (if (consp tree)
	      (or (rec-find-if predicate (first tree))
		  (rec-find-if predicate (rest tree)))))))

(defun node-equal-p (x node)
  (if (and (consp node) (eql (car node) x))
      node))

(defun find-node (type tree)
  (if (consp tree) 
      (or (node-equal-p type tree)
	  (find-node type (first tree))
	  (find-node type (rest tree)))))

(defun find-tide-table (page)
  (rec-find-if #'(lambda (x) 
		   (if (and (consp x)
			    (eq (first x) :table) ; table node
			    (eql 3 (length x))
			    (eq (first (third x)) :tbody) ; with tbody
			    (>= (length (third x)) (+ 2 5)) ; 2 header & 5+ data rows
					; (fourth (third x)) is second header row 
					; with "Time" & "Ht" labels 
					; and expect 14 columns in the row
			    (= (length (fourth (third x))) (+ 2 14)) ;
			    (equal (third (third (fourth (third x))))
				   "Time")
			    (equal (third (fourth (fourth (third x))))
				   "Ht"))
		       x)) page))
		      		      
(defun date-matches-p (header-row query-date)
  "Ensure the date in the header row matches the date in the query."
  (equal (parse-integer (third (third (third header-row)))
			:start 3 :junk-allowed t)
	 query-date))

(defun validate-tide-table (page date)
  (let* ((table (find-tide-table page))
	 (tbody (cddr (third table)))
	 (data (cddr tbody)))
    (declare (ignore data))
    (date-matches-p (first tbody) date)
    table))

(defun html-tide-table-to-stream (port-name year month day stream)
  "Get html table for tides for the location for 7 days from the year, month, and day, and write it to the stream. Port name is the name of the port, for example Brisbane Bar, Yamba, Urangan (see http://www.bom.gov.au/oceanography/tides/MAPS/qld.shtml). Year is a four-digit integer. Month is a 3-character string such as Jan or Feb. Day is an integer from 1 to 31."
  (let* ((port (standard-port port-name))
	 (port-code (first port))
	 (port-state (standard-port-state port))
	 (uri (expand-uri-template "http://www.bom.gov.au/cgi-bin/oceanography/tides/tide_predications.cgi?location={port-code}&Submit.x=63&Submit.y=4&tide_hiddenField={port-state}&years={year}&months={month}&dates={day}")))
    (serialize-lhtml (validate-tide-table (parse (http-request uri) 
						 (make-lhtml-builder)) day)
		     (make-character-stream-sink stream))))
 
