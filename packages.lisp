
(in-package :cl-user)

(defpackage :au-bom-tides
  (:use :cl 
	:closure-html
	:drakma
	:cl-uri-templates
	:local-time
	:iterate)
  (:import-from :cl-utilities :split-sequence :compose)
  (:import-from :alexandria :curry)
  (:import-from :anaphora :awhen :aif :it)
  (:documentation
   "Genospatial data and algorithms."))
