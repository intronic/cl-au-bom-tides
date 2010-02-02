;;;; file: http.lisp
;;;; 
;;;; Function library with HTTP protocol helper functions for CommonLisp
;;;;
;;;; Copyright (C) 2003,2004 Alexander Schreiber
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation;
;;;; version 2 of the License.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the Free
;;;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;
;;;; author : Alexander Schreiber <als@thangorodrim.de>
;;;; version: $Id: http.lisp 1728 2008-02-17 16:16:22Z als $
;;;;
;;;;
;;;; design note: Since this code really is only useful in a CGI environment
;;;;              and pretty much nowhere else, I happily read/write only
;;;;              to/from std(in|out) with no option to change this.


(defpackage :http
  (:use :common-lisp)
  (:documentation
   "A simple interface to HTTP for CGI programming, mainly offerering access
     to environment variables, query strings, parsed query parameters
     and cookies.")
  (:export #:http-add-header
           #:http-send-headers
           #:http-get-env-vars
           #:http-get-query-string
           #:http-parse-query-string
           #:http-query-parameter
           #:http-query-parameter-list
	   #:http-getenv
           #:http-add-cookie
           #:http-cookie
           #:http-cookie-list
           #:http-timestamp
           #:http-dump-query-parms
           #:http-init
           #:*http-url-encoding-charset*))

(in-package :http)


(eval-when (:compile-toplevel)
  (proclaim '(optimize (safety 3)))) ; this should be the default

;; in case of SBCL, shut the compiler up

#+sbcl
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))



;;; globals

(defparameter *http-library-version* "$Revision: 1728 $" "revision of library")

(defparameter *http-env-vars*
  '("CONTENT_LENGTH"
    "CONTENT_TYPE"
    "DOCUMENT_ROOT"
    "GATEWAY_INTERFACE"
    "HTTP_ACCEPT"
    "HTTP_ACCEPT_CHARSET"
    "HTTP_ACCEPT_ENCODING"
    "HTTP_ACCEPT_LANGUAGE"
    "HTTP_CONNECTION"
    "HTTP_COOKIE"
    "HTTP_HOST"
    "HTTP_KEEP_ALIVE"
    "HTTP_REFERER"
    "HTTP_USER_AGENT"
    "PATH"
    "QUERY_STRING"
    "REMOTE_ADDR"
    "REMOTE_PORT"
    "REQUEST_METHOD"
    "REQUEST_URI"
    "SCRIPT_FILENAME"
    "SCRIPT_NAME"
    "SERVER_ADDR"
    "SERVER_ADMIN"
    "SERVER_NAME"
    "SERVER_PORT"
    "SERVER_PROTOCOL"
    "SERVER_SIGNATURE"
    "SERVER_SOFTWARE")
  "list of HTTP environment variables set by the webserver for the CGI")

(defvar *http-pending-headers* ()
  "HTTP headers pending write, list of (header value) pairs, don't touch")

(defvar *http-headers-done* nil "HTTP headers already sent?")

(defvar *http-query-string* nil "The cached HTTP query string")

(defvar *query-string-parsed* nil "HTTP query string already parsed?")

(defvar *http-query-parameters* (make-hash-table :test #'equal)
  "The parsed HTTP query parameters")

(defvar *query-arguments* (make-hash-table :test #'equal) 
  "parsed query arguments, empty upon startup, needs to be filled by 
parse-query-string")

(defvar *cookie-jar* (make-hash-table :test #'equal) 
  "A place for cookies found in the requests")

(defvar *read-cookies-done* nil "reading of cookie data done?")

(defvar *cookie-name-xlator* (make-hash-table :test #'equal)
  "translator for case insensitive cookie lookup (according to RFC2109)")

(defconstant  +hex-char-digits+ "0123456789ABCDEF" "valid digits of 
hexadecimal numbers, inorder")

(defvar *http-url-encoding-charset* :utf-8
  "Charset used to url-decode HTTP query, url-encode cookies. One of
:latin1 (alias :iso-8859-1), :latin9 (alias :iso-8859-9), :utf-8")


(defconstant +url-encode-ok-chars+ '( #\. #\/) 
  "Characters that are ok in URL-encoding and need not be encoded.
   Note: alphanumerics are automagically assumed to not need encoding.")

;;; potentially implementation dependent functions
;;; these are wrappers that might need changing for your CL implementation
#+clisp
(defun http-getenv (variable)
  "get value of the named environment variable"
  (ext:getenv variable)) ; works for CLISP 

#+sbcl
(defun http-getenv (variable)
  "get value of the named environment variable"
  (sb-ext:posix-getenv variable))

#+gcl
(defun http-getenv (variable)
  "get value of the named environment variable"
  (system:getenv variable))

;;; CMU CL demands a bit more work
#+cmu
(defun http-getenv (variable)
  "get value of the named environment variable"
  (cdr (assoc (intern variable :keyword)
                            ext:*environment-list*)))

#+ecl
(defun http-getenv (variable)
    "get value of the named environment variable"
      (si:getenv variable))

;;; support for Lispworks thanks to Bob Hutchison <hutch@recursive.ca>
;;; he tested it with Lispworks Pro 4.4.5 on Mac
#+lispworks
(defun http-getenv (variable)
    "get value of the named environment variable"
    (lispworks:environment-variable variable))

#+allegro
(defun http-getenv (variable)
  "get value of the named environment variable"
  (sys:getenv variable))

;;;; functions (plain CommonLisp)


(defun http-get-env-vars () 
  "Processes *http-env-vars*, tries to get values for all environment
variables mentioned there, stuffs them into a hash and returns this hash."
  (let ((env-vars (make-hash-table :test #'equal)))
    (dolist (var *http-env-vars*)
      (setf (gethash var env-vars) (http-getenv var)))
    env-vars))


(defun http-add-header (name value)
  "add a HTTP header pair (name & value) to the list of HTTP headers
to be sent - unless they have already been sent"
  (unless *http-headers-done*  ; no sense adding new ones after sending
    (setf *http-pending-headers* 
          (append *http-pending-headers* (list name value)))))
      
 
(defun http-send-headers (&optional (content-type
                                     "text/html; charset=UTF-8"))
  "write any additional HTTP-headers and the mandatory HTTP Content-Type 
header - unless they have already been sent"
  (unless *http-headers-done* ; sending them twice would be ... bad
    (format t "~&~{~A: ~A~%~}" *http-pending-headers*)
    (format t "Content-Type: ~a~%~%" content-type)
    (setq *http-headers-done* t)))



(defun http-get-query-string ()
  "Returns the HTTP QUERY_STRING. Uses the value cached in *http-query-string*
or, if it is empty, reads the POST/GET value, stores it in *http-query-string*
and returns it. Only handles simple GET or POST with content type
application/x-www-form-url-encoded"
  (unless *http-query-string*
    (setq *http-query-string*
          (cond ((string-equal (http-getenv "REQUEST_METHOD") "POST")
                 (read-line *standard-input* nil ""))
                (t  ; assume GET data by default
                 (http-getenv "QUERY_STRING")))))
  *http-query-string*)


(defun hex-digit-char (hex-digit)
  "returns the appropriate character for the specified hexadecimal
digit, nil if the specified value is outside 0 .. 15"
  (when (and (< hex-digit 16) (> hex-digit -1))
    (char +hex-char-digits+ hex-digit)))
      


(defun hex-char-digit (hex-char)
  "returns the decimal digit for the specified hex char, nil if it
is not a hexadecimal char (not in 0 .. 9 a .. f). Works case-insensitive"
  (position (char-upcase hex-char) +hex-char-digits+))


(defun url-ok-char-p (testchar)
  "returns T if testchar is ok for verbatim printing in URLs, NIL if not"
  (cond ((alphanumericp testchar) t)
        ((member testchar +url-encode-ok-chars+) t)
        ( t nil)))


;; the following three functions:
;;  - implementation-name-for-encoding
;;  - convert-string-to-bytes
;;  - convert-string-from-bytes
;; have been copied from Eric Marsdens "pg.lisp" project, specifically
;; the file sysdep.lisp


(defun implementation-name-for-encoding (encoding)
  #+(and clisp unicode)
  (case encoding
    ((:latin1 :iso-8859-1) charset:iso-8859-1)
    ((:latin9 :iso-8859-9) charset:iso-8859-9)
    (:utf-8 charset:utf-8)
    (otherwise (error "unknown encoding ~A" encoding)))
  #+(and allegro ics)
  (case encoding
    ((:latin1 :iso-8859-1) :latin1)
    ((:latin9 :iso-8859-9) :latin9)
    (:utf-8 :utf8)
    (otherwise (error "unknown encoding ~A" encoding)))
  #+(and sbcl sb-unicode)
  (case encoding
    ((:latin1 :iso-8859-1) :latin1)
    ((:latin9 :iso-8859-9) :latin9)
    (:utf-8 :utf8)
    (otherwise (error "unknown encoding ~A" encoding)))
  #+(or cmu gcl ecl abcl openmcl)
  nil)


(defun convert-string-to-bytes (string encoding)
  (declare (type string string))
  (format *error-output* "string |~a| encoding |~a|~%" string encoding) ; debug
          
  #+(and clisp unicode)
  (ext:convert-string-to-bytes string
                               (implementation-name-for-encoding
                                encoding))
  #+(and allegro ics)
  (excl:string-to-octets string
                         :null-terminate nil
                         :external-format
                         (implementation-name-for-encoding encoding))
  #+(and :sbcl :sb-unicode)
  (sb-ext:string-to-octets string
                           :external-format
                           (implementation-name-for-encoding encoding))
  #+(or cmu gcl ecl abcl openmcl)
  (if (member encoding '(:latin1 :iso-8859-1 :latin9 :iso-8859-9))
      (let ((octets (make-array (length string) :element-type
                                '(unsigned-byte 8))))
        (map-into octets #'char-code string))
      (error "Can't convert ~A string to octets" encoding)))



(defun convert-string-from-bytes (bytes encoding)
  (declare (type (vector (unsigned-byte 8)) bytes))
  #+cmu
  (declare (ignore encoding))
  #+(and clisp unicode)
  (ext:convert-string-from-bytes bytes (implementation-name-for-encoding
                                        encoding))
  #+(and allegro ics)
  (excl:octets-to-string bytes
                         :external-format
                         (implementation-name-for-encoding encoding))
  #+(and :sbcl :sb-unicode)
  (sb-ext:octets-to-string bytes
                           :external-format
                           (implementation-name-for-encoding encoding))
  ;; for implementations that have no support for character
  ;; encoding, we assume that the encoding is an octet-for-octet
  ;; encoding, and convert directly
  #+(or cmu (and sbcl (not :sb-unicode)) gcl ecl abcl openmcl)
  (let ((string (make-string (length bytes))))
    (map-into string #'code-char bytes)))



(defun url-escape-char (char charset)
  (let ((bytes (convert-string-to-bytes (string char) charset)))
    (with-output-to-string (out)
      (map nil
           #'(lambda (byte) (format out "%~2,'0X" byte))
           bytes))))



(defun url-encode-string (input-string
                          &optional (charset
                                     *http-url-encoding-charset*))
  "URL-encodes the input-string and returns an url-encoded output-string"
  (let ((work-stream (make-string-output-stream)))
    (dotimes (position (length input-string)
              (get-output-stream-string work-stream))
      (if (url-ok-char-p (char input-string position))
          (princ (char input-string position) work-stream) ; copy verbatim
          (princ (url-escape-char (char input-string position) charset)
                 work-stream)))))




(defun url-decode-bytes (input-string)
  "Decode bytes coming in as URL-encoded string."
  (let ((byte-vect (make-array (length input-string)
                               :element-type '(unsigned-byte 8)
                               :fill-pointer 0))
        (num-low 0)
        (num-high 0)
        (skip 0))
    (dotimes (position (length input-string))
      (cond ((= skip 1) (setf num-low (hex-char-digit
                                       (char input-string position)))
             (vector-push (+ (* num-high 16) num-low) byte-vect)
             (setf skip (- skip 1)))
            ((= skip 2) (setf num-high (hex-char-digit
                                        (char input-string position)))
             (setf skip (- skip 1)))
            (t (cond ((char= (char input-string position) #\+) ; space
                      (vector-push (char-code #\Space) byte-vect))
                     ((char= (char input-string position) #\%) ; hexdigit?
                      (if (and
                           (>= (- (length input-string) position) 3)
                           (hex-char-digit (char input-string (+ position
                                                                 1)))
                           (hex-char-digit (char input-string (+ position
                                                                 2))))
                          (setf skip 2) ; hexdigits to process follow
                          
                          ;; Not escaped characters are assumed to have codes
                          ;; that fit into one byte and have the same
                          ;; representation in all charsets.
                          (vector-push (logand #xff
                                               (char-code
                                                (char input-string position)))
                                       byte-vect)))
                     (t (vector-push (logand #xff
                                             (char-code
                                              (char input-string position)))
                                     byte-vect))))))
    byte-vect))




(defun url-decode-string (input-string &optional (charset
                                                  *http-url-encoding-charset*))
  "URL-decodes the input-string and returns an url-decoded strings, deals
correctly with partly encoded (parts encoded and parts plain) strings.
Invalid hex-code sequences are ignored and printed through verbatim."
  (convert-string-from-bytes (url-decode-bytes input-string)
                             charset))





(defun add-item-to-hash! (hash key value)
  "Adds an item to the named hash. Upon encountering and existing value,
automagically does chaining (turns the simple value into a list of values).
Note: modifies the hash in-place."
  (cond ((null (gethash key hash))  ; nothing here, make simple value
         (setf (gethash key hash) value))
        ((listp (gethash key hash)) ; already chained, append
         (setf (gethash key hash)
               (append (gethash key hash)
                       (list value))))
        (t                          ; plain value, make a chain
         (setf (gethash key hash)
               (append (list (gethash key hash))
                       (list value)))))
  hash)
                


(defun http-parse-query-string (&optional query-string)
  "Parses the provided query string into key-value groups and puts that
into a hash, returns the hash."
;;; example: "foo=bar&answer=42&question=wanted"
  (unless *query-string-parsed* 
    (unless query-string (setf query-string (http-get-query-string)))
    (let ((key nil)
          (value nil)
          (pointer 0))
      (dotimes (position (length query-string))
        (cond
          ((char= (char query-string position) #\=)
           (setf key (subseq query-string pointer position))
           (setf pointer (1+ position)))
          ((char= (char query-string position) #\&)
           (setf value (subseq query-string pointer position))
           (setf pointer (1+ position))
           (add-item-to-hash! *http-query-parameters*
                              (url-decode-string key)
                              (url-decode-string value))
           (setf value nil))))
      (when (and (< pointer (length query-string)) (null value))
        (add-item-to-hash! *http-query-parameters* 
                           (url-decode-string key)
                           (url-decode-string 
                            (subseq query-string pointer))))
      (setf *query-string-parsed* t)
      *http-query-parameters*)))


(defun http-query-parameter (name)
  "Return the value of the named HTTP query parameter."
  (unless *query-string-parsed*
    (http-parse-query-string))
  (gethash name *http-query-parameters*))




(defun http-get-content-type () 
  "Tries to get the content type from the environment, returns either the
content-type string or NIL."
  (let ((cti (http-getenv "CONTENT_TYPE"))) ; cache the content-type string
    (if (string= "" cti)
	(if (position #\; cti)
	    (subseq cti 0 (position #\; cti))))))


(defun http-get-multipart-boundary ()
  "Tries to get the multipart boundary string from the content type, returns
either the corrent boundary string or NIL."
  (let ((cti (http-getenv "CONTENT_TYPE"))) ; cache the content-type string
    (if (search "multipart" cti)
	(if (search "boundary=" cti)
	    (concatenate 'string
                         "--" 
                         (subseq cti (+ (search "boundary=" cti) 1)))))))


(defun http-dump-query-parms ()
  "returns string of HTTP query parameters." 
  (maphash #'(lambda (key value) 
               (format t "~&<li><tt>~a = ~a</tt></li>~%" key value ))
           (http-parse-query-string )))


(defun needs-quoting (test-string)
  "Tests if a string needs quoting by checking if it contains non-alphanumeric
characters, returns T or NIL"
  (if (< 0 (length (remove-if #'alphanumericp test-string))) 
      t))


(defun http-add-cookie (name value &key 
                        (Comment nil)
                        (Domain nil)
                        (Max-Age nil)
                        (Path nil)
                        (Secure nil)
                        (Version "1"))
  "Add a RFC2109 cookie with the specified data to the list of headers to
 send. Modify the Version argument at your own peril.
   Note: http-add-cookie _must_ be called _before_ calling http-send-headers
         because cookies are transmitted as part of the HTTP headers."
  (http-add-header "Set-Cookie"
                   (concatenate 'string
                                (format nil "~a=~a"
                                        name 
                                        (if (needs-quoting value)
                                            (format nil "\"~a\"" value)
                                            value))
                                (unless (null Comment)
                                  (format nil "; Comment=\"~a\"" Comment))
                                (unless (null Domain)
                                  (format nil "; Domain=\"~a\"" Domain))
                                (unless (null Max-Age)
                                  (format nil "; Max-Age=~a" Max-Age))
                                (unless (null Path)
                                  (format nil "; Path=~a"
                                          (url-encode-string Path)))
                                (unless (null Secure)
                                  (format nil  "; Secure"))
                                (format nil  "; Version=\"~a\""
                                        Version))))


(defun read-cookies ()
  "Read all cookies from the request and store them into the cookie jar."
;;; example: foo=bar; quux="super baz"; grumble="Mr. \"RockIt\" Joe"
;;;
;;; This is going to be a primitive state machine and the code will be 
;;; rather ugly. 
;;; But it works, so I'll leave it for the time being.
  (unless (null (http-getenv "HTTP_COOKIE"))
    (let ((cookies (http-getenv "HTTP_COOKIE"))
          (key nil)
          (value nil)
          (work nil)
          (is-key t)
          (is-value nil)
          (is-to-be-quoted nil)
          (open-quote nil))
      (dotimes (position (length cookies))
        (cond
          ((and (char= (char cookies position) #\=) ; end-of-name
                is-key)
           (setf key (remove-if #'(lambda (item)
                                    (char= #\Space item))
                                work))
           (setf work nil)
           (setf is-key nil)
           (setf is-value t))
          ((and (char= (char cookies position) #\") ; start/end-of-quoted-value
                is-value
                (not is-to-be-quoted))
           (if open-quote
               (setf open-quote nil)
               (setf open-quote t))
           (setf is-value t))
          ((and (char= (char cookies position) #\\) ; quote next char
                is-value
                (not is-to-be-quoted))
           (setf is-to-be-quoted t))
          ((and (or (char= (char cookies position) #\;)
                    (char= (char cookies position) #\,))
                (not open-quote)
                (not is-to-be-quoted)
                is-value)
           (setf value work)
           (unless (gethash key *cookie-jar*)
             (setf (gethash key *cookie-jar*) value))
           (setf (gethash (string-downcase key) *cookie-name-xlator*) key)
           (setf work nil)
           (setf is-value nil)
           (setf is-key t))

          (t                     ; nothing special, copy to buffer
           (setf work 
                 (concatenate 'string work 
                              (string (char cookies position))))
           (setf is-to-be-quoted nil))))
      (unless (null key)
        (setf value work)
        (unless (gethash key *cookie-jar*)
          (setf (gethash key *cookie-jar*) value)
          (setf (gethash (string-downcase key) *cookie-name-xlator*) key))))))


(defun http-cookie (name)
  "Fetch the value of the specified cookie. Case insensitive. Returns NIL
if the cookie is not found."
  (unless *read-cookies-done*
    (read-cookies))
  (if (gethash name *cookie-jar*)
      (gethash name *cookie-jar*)
      (gethash (gethash (string-downcase name) 
                        *cookie-name-xlator*) 
               *cookie-jar*)))


(defun http-cookie-list ()
  "Returns a list of all stored cookies received from the user agent."
  (unless *read-cookies-done*
    (read-cookies))
  (let ((cookies '()))
    (maphash (lambda (key value)
               (declare (ignore value))
               (setf cookies 
                     (append cookies (list key))))
             *cookie-jar*)
    cookies))


(defun http-query-parameter-list ()
  "Returns a list of all stored HTTP query parameters."
  (unless *query-string-parsed* 
    (http-parse-query-string))
  (let ((query-parameters '()))
    (maphash (lambda (key value)
               (declare (ignore value))
               (setf query-parameters
                     (append query-parameters (list key))))
             *http-query-parameters*)
    query-parameters))


(defun http-timestamp (&optional (timeval nil))
  "Returns a HTTP timestamp (see RFC2616). Expects timeval to be universal
time or NIL, assumes current time if NIL."
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (if (null timeval)
                                 (get-universal-time)
                                 timeval))
    (declare (ignore daylight-p))
    (format nil "~A, ~2,'0D ~A ~D ~2,'0D:~2,'0D:~2,'0D ~D"
            (nth day '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
            date
            (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                              "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
            year
            hour minute second
            zone)))


(defun http-init ()
  "Init function, runs fixups where needed."
  ;; SBCL fixup: For some reason, SBCL initially opened the
  ;; *standard-output* stream with :external-format :ascii
  ;; when running as CGI. If we are running with another encoding
  ;; (like, UTF8) this is ... less than optimal. So we are pulling
  ;; a dirty trick that very likely only works on Linux.
  #+sbcl
  (if (not (equal (stream-external-format *standard-output*)
                  *http-url-encoding-charset*))
      (if (string-equal (software-type) "Linux")
          (setf *standard-output*
                (open "/dev/stdout"
                      :external-format *http-url-encoding-charset*
                      :direction :output
                      :if-exists :append
                      :if-does-not-exist :error))))

  t) ; do _something_ on other platforms *g*
