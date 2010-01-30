
(in-package :cl-user)

(defpackage :au-bom-tides
  (:use :cl 
	:closure-html
	:drakma
	:cl-uri-templates
	:local-time
	:iterate)
  (:import-from :cl-utilities :split-sequence)
  (:import-from :alexandria :curry)
  (:import-from :anaphora :awhen :it)
  (:documentation
   "Genospatial data and algorithms."))
