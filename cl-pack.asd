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


(defsystem #:cl-pack
  :name "cl-pack"
  :author "Dan Ballard <dan@mindstab.net>"
  :version "0.2"
  :licence "BSD-3-Clause"
  :description "Perl compatible binary pack() and unpack() library"
  :depends-on (:ieee-floats)
  :components ((:file "package")
               (:file "cl-pack")))


(defsystem #:cl-pack-test
  :depends-on (:cl-pack)
  :components ((:file "tests")))
