(require :au-bom-tides)
(save-lisp-and-die "sbcl.core.cgi" :toplevel #'au-bom-tides::cgi-handler :executable t)
