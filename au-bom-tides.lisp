(in-package :au-bom-tides)

;;;; http://www.bom.gov.au/cgi-bin/oceanography/tides/tide_predications.cgi?location=qld_59980&Submit.x=63&Submit.y=4&tide_hiddenField=Queensland&years=2010&months=Jan&dates=31

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

(defun find-rows (page)
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

(defun parse-page (page date)
  (let* ((table (find-rows page))
	 (tbody (cddr (third table)))
	 (data (cddr tbody)))
    (date-matches-p (first tbody) date)
    table))

