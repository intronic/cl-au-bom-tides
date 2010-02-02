(in-package :au-bom-tides)

;;; text utililies
(defun parse-decimal (str)
  (let* ((parts (split-sequence #\. str))
         (a (car parts))
         (b (cadr parts))
         (a1 (parse-integer a))
         (b1 (or (parse-integer (or b "0") :junk-allowed t) 0)))
    (+ a1 (/ b1 (expt 10 (length b))))))

(defun parse-decimal-or-null (string)
  (if string (parse-decimal string)))

(defun trim-whitespace (s)
  (string-trim '(#\Space #\Tab #\Newline #\NO-BREAK_SPACE) s))

(defun trim-whitespace-to-null (s)
  (let ((trim (trim-whitespace s)))
    (if (plusp (length trim))
	trim)))

;;; date functions
(defun short-month-name (month)
  (aref +short-month-names+ month))

(defun first-day (year)
  "First day of the year."
  (encode-timestamp 0 0 0 0 1 1 year))

(defun add-days (date days)
  "Add days to date."
  (adjust-timestamp date (offset :day days)))

;;; Recursive find 
(defun rec-find-if (predicate tree)
  (if (null tree)
      nil
      (or (funcall predicate tree)
	  (if (consp tree)
	      (or (rec-find-if predicate (first tree))
		  (rec-find-if predicate (rest tree)))))))

;;;http://lib.store.yahoo.net/lib/paulgraham/acl2.lisp
(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))
