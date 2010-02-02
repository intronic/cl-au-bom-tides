;;;; file: html.lisp
;;;; 
;;;; Function library for generating HTML output from CommonLisp.
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
;;;; version: $Id: html.lisp 1728 2008-02-17 16:16:22Z als $
;;;;


(defpackage :html
  (:use :common-lisp)
  (:documentation
   "A simple HTML generating package. Offers standard functions for most
 HTML tags (some are missing) and special function/macros for different
jobs.")
  (:export #:a #:br #:hr 
           #:h1 #:h2 #:h3 #:h4 #:h5 #:h6
           #:body #:head #:html #:title
           #:img
           #:center
           #:font #:basefont
           #:ins #:del
           #:ol #:ul #:li #:dl #:menu #:dir
           #:p
           #:em #:strong #:code #:samp #:kbd #:var #:cite #:dfn 
           #:acronym #:abbr
           #:table #:thead #:tfoot #:tbody #:tr #:td #:th #:col #:colgroup
           #:address 
           #:frameset #:frame #:noframes #:iframe
           #:object #:applet
           #:script #:noscript
           #:div #:span
           #:pre #:blockquote
           #:html-header #:html-footer
           #:form #:form-reset-submit #:form-hidden #:form-checkbox
           #:form-self-post
           #:form-select #:form-select-multiple #:form-textarea
           #:input #:textarea #:button #:fieldset #:legend #:label
           #:force-linebreaks
           #:label))

(in-package :html)

(eval-when (:compile-toplevel)
  (proclaim '(optimize (safety 3) (speed 3))))


;; (defconstant +html-header+ 
;;  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\">
;;<html>"
;;  "default HTML header")


(defconstant +html-header+
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">

<html xmlns=\"http://www.w3.org/1999/xhtml\">"
  "default html header")

;;; macro to generate generic HTML tag functions
;;; the generated functions take a list of string arguments and print 
;;; them separated by newline, surrounded by the appropriate opening
;;; and closing tags

(defmacro def-html-fun (tag &key start-with-newline (close-tag t))
  "generate a function to return a HTML tagged string"
  `(defun ,tag (&rest args)
    (let ((attr (if (consp (first args)) (first args)
                    nil))
          (content (if (consp (first args)) (rest args)
                       args)))
      (apply #'concatenate 'string
             (append
              ,(if start-with-newline
                   `(list (string #\Newline)))
              (list "<" (string-downcase (symbol-name ',tag)))
              (unless (null attr)
                (list (apply #'concatenate 'string
                             (append 
                              (mapcar #'(lambda (pair)
                                          (concatenate 
                                           'string
                                           " "
                                           (if (stringp (car pair)) (car pair)
                                               (string-downcase (car pair)))
                                           "=\""
                                           (string (cdr pair))
                                           "\""))
                                      attr)))))
              (list ">")
              content
              ,(if close-tag
                   `(list "</" (string-downcase (symbol-name ',tag)) ">"))
              (list (string #\Newline)))))))



;;; generate misc. generic HTML tag functions
;;; highly incomplete list, more will be added as needed


(def-html-fun html)
(def-html-fun head :start-with-newline t)
(def-html-fun body :start-with-newline t)
(def-html-fun title)

(def-html-fun h1 :start-with-newline t)
(def-html-fun h2 :start-with-newline t)
(def-html-fun h3 :start-with-newline t)
(def-html-fun h4 :start-with-newline t)
(def-html-fun h5 :start-with-newline t)
(def-html-fun h6 :start-with-newline t)

(def-html-fun table :start-with-newline t)
(def-html-fun tr :start-with-newline t)
(def-html-fun thead :start-with-newline t)
(def-html-fun tfoot :start-with-newline t)
(def-html-fun tbody :start-with-newline t)
(def-html-fun td)
(def-html-fun th :start-with-newline t)
(def-html-fun col)
(def-html-fun colgroup :start-with-newline t)

; logical text markup
(def-html-fun em)
(def-html-fun strong)
(def-html-fun code)
(def-html-fun samp)
(def-html-fun kbd)
(def-html-fun var)
(def-html-fun cite)
(def-html-fun dfn)
(def-html-fun acronym)
(def-html-fun abbr)

(def-html-fun p :start-with-newline t)

(def-html-fun center :start-with-newline t)

(def-html-fun ol :start-with-newline t)
(def-html-fun ul :start-with-newline t)
(def-html-fun dl :start-with-newline t)
(def-html-fun menu :start-with-newline t)
(def-html-fun dir :start-with-newline t)
(def-html-fun li)

(def-html-fun a)
(def-html-fun img)

(def-html-fun ins)
(def-html-fun del)

(def-html-fun font)
(def-html-fun basefont)

; frame-crap
(def-html-fun frameset :start-with-newline t)
(def-html-fun noframes :start-with-newline t)
(def-html-fun frame :start-with-newline t)
(def-html-fun iframe :start-with-newline t)

; JavaScript
(def-html-fun script :start-with-newline t)
(def-html-fun noscript :start-with-newline t)

; embedded multimedia objects/applets
(def-html-fun object :start-with-newline t)
(def-html-fun applet :start-with-newline t)


(def-html-fun address)

(def-html-fun br :start-with-newline t)
(def-html-fun hr :start-with-newline t)

(def-html-fun div)
(def-html-fun span)

(def-html-fun pre :start-with-newline t)
(def-html-fun blockquote :start-with-newline t)

;;; form handling

(def-html-fun form :start-with-newline t)
(def-html-fun input :start-with-newline t :close-tag nil)
(def-html-fun textarea :start-with-newline t)
(def-html-fun button :start-with-newline t)
(def-html-fun fieldset :start-with-newline t)
(def-html-fun legend :start-with-newline t)
(def-html-fun label :start-with-newline t)

;;; specialiced functions

(defun html-header (title &key stylesheet made metatags)
    "return html head section with page title and optional
stylesheet, made entry, meta tag list, ends with an open body tag."
    (let ((buf (make-string-output-stream)))
      (check-type metatags list)
      (format buf "~a~%<head>~%" +html-header+)
      (unless (null stylesheet) ; handle stylesheet argument
        ;; first, handle old browsers
        (format buf "<link href=\"~a\" rel=\"stylesheet\" " stylesheet )
        (format buf "type=\"text/css\">~%")
        ;; then, handle new browsers
        (format buf "<style type=\"text/css\" media=\"screen\">~%")
        (format buf "  <!-- @import url(~a) screen; " stylesheet)
        (format buf "-->~%</style>~%"))
      (unless (null made) ; handle made argument
        (let ((line "<link rev=\"made\" "))
          (princ line buf)
          (format buf "href=\"~A\">~%" made)))
      (unless (null metatags) ; handle meta tag list
        (dolist (entry metatags)
          (format buf "<meta name=\"~A\" content=\"~A\">~%" 
                  (first entry) (second entry))))
      (format buf "<meta http-equiv=\"Content-Type\" ")
      (format buf "content=\"text/html; charset=UTF-8\">~%")
      (format buf "<title>~A</title>~%" title)
      (format buf "</head>~%")
      (get-output-stream-string buf)))



(defun html-footer (&key (body-close nil))
    "Return body and html closing tags. Note: closing body tag 
defaults to off."
    (concatenate 'string 
                 (if body-close
                     (format nil "</body>~%"))
                 (format nil "</html>~%")))



(defmacro form-reset-submit (&key (reset "reset") (submit "submit"))
  "simple macro for generating the usual reset/submit button pair with
 custom writing on them."
  `(concatenate 'string 
    (html:input '((type . "reset")
                  (value . ,reset)))
    (html:input '((type . "submit")
                  (value . ,submit)))))

(defun form-hidden (pairlist)
  "Function to return hidden value entries for forms. Accepts
 property lists and returns one hidden value entry for each
 key/value pair encountered."
  (apply #'concatenate 'string
         (mapcar #'(lambda (pair)
                     (concatenate 'string
                                  "<input type=\"hidden\" "
                                  "name=\""
                                  (if (symbolp (car pair))
                                      (string-downcase (car pair))
                                      (format nil "~a" (car pair)))
                                  "\" value=\""
                                  (format nil "~a" (cdr pair))
                                  "\">"
                                  (string #\Newline)))
                 pairlist)))


(defun form-checkbox (pair &rest text)
  "Returns HTML for a checkbox for forms, uses a cons for key-value."
  (concatenate 'string
               "<input type=\"checkbox\" name=\""
               (car pair)
               "\" value=\""
               (cdr pair)
               "\">"
               (apply #'concatenate 'string text)
               (string #\Newline)))


(defun form-select (name entries &optional selected)
  "Returns HTML for a form select field with the provided name and list
of elements."
  (concatenate 'string
               (format nil "<select name=\"~a\">~%" name)
               (apply #'concatenate 'string
                      (mapcar #'(lambda (item)
                                  (if (string= (format nil "~a" item)
                                               (format nil "~a" selected))
                                      (concatenate 
                                       'string
                                       "<option selected=\"selected\">"
                                       (format nil "~a" item)
                                       (format nil "</option>~%"))
                                      (format nil "<option>~a</option>~%" 
                                              item)))
                              entries))
               (format nil "</select>~%")))

(defun form-select-multiple (name entries &optional selected)
  "Returns HTML for a form select field with the provided name and list
of elements."
  (concatenate 'string
               (format nil "<select multiple size=\"3\" name=\"~a\">~%" name)
               (apply #'concatenate 'string
                      (mapcar #'(lambda (item)
                                  (if (string= (format nil "~a" item)
                                               (format nil "~a" selected))
                                      (concatenate 
                                       'string
                                       "<option selected=\"selected\">"
                                       (format nil "~a" item)
                                       (format nil "</option>~%"))
                                      (format nil "<option>~a</option>~%" 
                                              item)))
                              entries))
               (format nil "</select>~%")))


(defun form-textarea (name cols rows &optional text)
  "Returns HTML for a form textarea with the supplied name and size (in
 columns and rows as well as optional predefined content."
  (declare (type fixnum cols rows))
  (declare (type string name))
  (concatenate 'string
               (format nil "<textarea name=\"~a\" cols=\"~a\" rows=\"~a\">~%"
                       name cols rows)
               (unless (null text)
                 text)
               (format nil "~&</textarea>~%")))


(defmacro form-self-post (&rest text)
    "Expands to form with action=$SCRIPT_NAME and METHOD=POST as parameters."
  `(html:form 
    (pairlis
     '("action" "method")
     (list (http:http-getenv "SCRIPT_NAME") "POST"))
    ,@text))



(defun force-linebreaks (original)
  "Forces linebreaks from the original be insert <br> tags."
  (let ((linebreaks 0))
    (dotimes (i (length original))
      (if (equal (char original i) #\Newline)
          (setf linebreaks (1+ linebreaks))))
    (let ((broken (make-string (+ (length original) (* 4 linebreaks))))
          (broken-pos 0))
      (dotimes (i (length original))
        (if (equal (char original i) #\Newline)
            (replace broken 
                     (concatenate 'string 
                                  "<br>" 
                                  (string #\Newline))
                     :start1 broken-pos
                     :end1 (setf broken-pos (+ broken-pos 5))
                     :start2 0
                     :end2 5)
            (replace broken 
                     original
                     :start1 broken-pos
                     :end1 (setf broken-pos (1+ broken-pos))
                     :start2 i
                     :end2 (1+ i))))
      broken)))

