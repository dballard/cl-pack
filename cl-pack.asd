;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; ******************************************************
;;;; FILE IDENTIFICATION
;;;; 
;;;; Name:     cl-pack.asd
;;;; Purpose:  System definition for CL-PACK
;;;; Author:   Dan Ballard <http://mindstab.net>
;;;; Created:  May 2009
;;;; License:  BSD
;;;; Description:  CL-PACK supplies perl/php/ruby compatible
;;;;               pack() and unpack() functions to allow
;;;;               easy use of binary protocols with the above
;;;;               mentioned languages and C.
;;;;*******************************************************


(defpackage #:cl-pack-system (:use #:asdf #:cl))
(in-package #:cl-pack-system)

; Try to find ieee-floats in the system,
; otherwise we'll load the copy we ship with
;(eval-when (:compile-toplevel :load-toplevel :execute)
(handler-case
    (unless (find-package 'ieee-floats)
      (progn
	(asdf:operate 'asdf:load-op 'ieee-floats)
	(push :native-ieee-floats *features*)))
  (MISSING-COMPONENT (e) nil))


(defsystem #:cl-pack
  :name "cl-pack"
  :author "Dan Ballard <haplo@mindstab.net>"
  :version "0.1"
  :licence "BSD"
  :description "perl compatible binary pack() and unpack() library"
  :depends-on #+native-ieee-floats(:ieee-floats)
              #-native-ieee-floats()
  :components (#-native-ieee-floats
	       (:module ieee-floats
			:components ((:file "ieee-floats")))
	       
               (:file "package"
                      :depends-on (ieee-floats))
	       (:file "cl-pack"
		      :depends-on ("package" ieee-floats))))
	       
(defsystem #:cl-pack-test
  :depends-on (:cl-pack)
  :components ((:file "tests")))
  
  
