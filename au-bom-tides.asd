(in-package :cl-user)

(defpackage :au-bom-tides-asd
  (:use :cl :asdf))


(in-package :au-bom-tides-asd)

(asdf:defsystem :au-bom-tides
  :serial t
  :name "Tides from bom.gov.au"
  :version "0.0.1"
  :maintainer ""
  :author ""
  :licence ""
  :description "Tides from bom.gov.au"
  :depends-on ( :cl-html-parse ) ; :hunchentoot :cl-utilities :fiveam :anaphora :cl-fad)
  :components ((:file "packages")
	       (:file "au-bom-tides"
		      :depends-on ("packages"))))
	       ;; (:module test
	       ;; 		:components ((:file "test"))
	       ;; 		:depends-on ("au-bom-tides"))))
