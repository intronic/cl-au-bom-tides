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
  :depends-on (:closure-html :drakma :cl-uri-templates :cl-utilities)
  :components ((:file "packages")
	       (:file "date-time"
		      :depends-on ("packages"))
	       (:file "au-bom-tides"
		      :depends-on ("date-time" "packages"))))
	       ;; (:module test
	       ;; 		:components ((:file "test"))
	       ;; 		:depends-on ("au-bom-tides"))))
