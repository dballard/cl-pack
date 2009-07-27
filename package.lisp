;;;; ******************************************************
;;;; FILE IDENTIFICATION
;;;; 
;;;; Name:     package.lisp
;;;; Purpose:  Package definition for CL-PACK
;;;; Author:   Dan Ballard <http://mindstab.net>
;;;; Created:  May 2009
;;;; License:  BSD
;;;; Description:  CL-PACK supplies perl/php/ruby compatible
;;;;               pack() and unpack() functions to allow
;;;;               easy use of binary protocols with the above
;;;;               mentioned languages and C.
;;;;*******************************************************

(in-package :common-lisp)

(defpackage #:cl-pack
  (:use #:common-lisp #:ieee-floats)
  (:export #:pack 
	   #:unpack))