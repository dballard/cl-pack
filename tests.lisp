;;;; ******************************************************
;;;; FILE IDENTIFICATION
;;;; 
;;;; Name:     tests.lisp
;;;; Purpose:  Tests for CL-PACK
;;;; Author:   Dan Ballard <http://mindstab.net>
;;;; Created:  May 2009
;;;; License:  BSD
;;;; Description:  CL-PACK supplies perl/php/ruby compatible
;;;;               pack() and unpack() functions to allow
;;;;               easy use of binary protocols with the above
;;;;               mentioned languages and C.
;;;;*******************************************************

(in-package :common-lisp)

(defpackage :cl-pack-test
  (:use #:common-lisp #:cl-pack)
  (:export #:test))

(in-package :cl-pack-test)

;;;; ***** Shamelessly ripped from Practical Common Lisp *****

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result pass-count total-count res pas tot)
    `(let ((,result t)
	   (,pass-count 0)
	   (,total-count 0))
       ,@(loop for f in forms collect 
	      `(multiple-value-bind (,res ,pas ,tot) ,f
		 (incf ,total-count (if ,tot ,tot 1))
		 (incf ,pass-count (if ,pas ,pas (if ,res 1 0)))
		 (if (not ,res) (setf ,result nil))))
       (format t "~a ~d/~d passed~%" *test-name* ,pass-count ,total-count)
       (values ,result ,pass-count ,total-count))))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;;;; **********************************************************


(deftest test ()
  (combine-results
   (pack-numbers)
   (pack-combinations)
   (pack-strings)
   (unpack-numbers)
   (unpack-combinations)
   (unpack-strings)
   (pack-signed) 
   (unpack-signed)
   (pack-form)
   (unpack-form)
   (mod-!)
   (mod-<>)))

(defun gen-null-string (len)
  (apply #'concatenate 'string (loop for i from 0 to (1- len) collecting (string #\null))))

(deftest pack-numbers ()
  (check
   (string= (pack "n" #x4142) "AB")
   (string= (pack "v" #x4142) "BA")
   (string= (pack "N" #x41424344) "ABCD")
   (string= (pack "V" #x41424344) "DCBA")
   (string= (pack "g" 15) (concatenate 'string "Ap" (string #\null) (string #\null)))
   (string= (pack "e" 15) (concatenate 'string (string #\null) (string #\null) "pA"))
   (string= (pack "G" 25) (concatenate 'string "@9" (gen-null-string 6)))
   (string= (pack "E" 25) (concatenate 'string (gen-null-string 6) "9@"))
   (string= (pack "x") (string #\null))
   (string= (pack "w" 193) (coerce `(,(code-char 129) #\A) 'string))
 ))


(deftest pack-combinations ()
  (check
   (string= (pack "c2" #x41 #x42 #x43) "AB") ;; basic repeater (with extra data dropped)
   (string= (pack "c*" #x41 #x42 #x43) "ABC") ;; basic * repeater
   (string= (pack "c3" #x41 #x42) "AB") ;; only use avail data -- ! should ERROR be raised?
   (string= (pack "c2N" #x41 #x42 #x43444546) "ABCDEF") ;; pick up after repeater
   (string= (pack "NX2" #x41424344) "AB") ;delete chars
   ))

(deftest pack-strings () 
  (check
    (string= (pack "a*" "Test String") "Test String") ;; * repeater with string data
    (string= (pack "a5" "1234") (concatenate 'string "1234" (string #\null))) ;; test null of 'a'
    (string= (pack "A10" "Test") "Test      ") ;; numeric repeater with string data and padding
    (string= (pack "a*N" "String" #x41424344) "StringABCD")  ;; pick up after string/*
    (string= (pack "A10V" "Test" #x41424344) "Test      DCBA") ;; pick up after string/#
    (string= (pack "c2xa*" #x41 #x42 "Test") (concatenate 'string "AB" (string #\null) "Test")) ;; non consuming 'x' plays nicely with others
    (string= (pack "B*" "010000010100001001000011") "ABC") ;; binary string
    (string= (pack "B16" "010000010100001001000011") "AB") ;; binary string only consumes what's asked of it
    (string= (pack "B*" "010000010100001") "AB") ;; AB string short a bit
    (string= (pack "b*" "100000101000010") "A!") ;; AB string short a bit
    (string= (pack "H4" "414243") "AB") ;; basic hex string (ignoring extra chars)
    (string= (pack "H*" "414") "A@") ;; padding right?
    (string= (pack "h*" "1424") "AB") ;; other byte ordering
    (string= (pack "Z*" "dan") (concatenate 'string "dan" (string #\null))) ;; null padded string *
    (string= (pack "Z5" "dan") (concatenate 'string "dan" (string #\null) (string #\null))) ;; padding of Z
    (string= (pack "Z3" "dan") (concatenate 'string "da" (string #\null))) ;; proper ending in NULL for under length string

    
    ))

(deftest unpack-numbers ()
  (check 
   (= (unpack "N" "ABCD") #x41424344)
   (= (unpack "V" "DCBA") #x41424344)
   (= (unpack "n" "AB") #x4142)
   (= (unpack "v" "BA") #x4142)
   (= (unpack "g" (concatenate 'string "Ap" (string #\null) (string #\null))) 15)
   (= (unpack "e" (concatenate 'string (string #\null) (string #\null) "pA")) 15)
   (= (unpack "G" (concatenate 'string "@9" (gen-null-string 6))) 25)
   (= (unpack "E" (concatenate 'string (gen-null-string 6) "9@")) 25)
   (= (unpack "w" (coerce `(,(code-char 129) #\A) 'string)) 193)
   ))

(deftest unpack-combinations ()
  (check
   (equal (multiple-value-list (unpack "c2" "ABC")) '(#x41 #x42)) ;; basic repeater (with extra data dropped)
   (equal (multiple-value-list (unpack "c*" "ABC")) '(#x41 #x42 #x43)) ;; basic * repeater
   (equal (multiple-value-list (unpack "c3" "AB")) '(#x41 #x42)) ;; only use avail data -- ! should ERROR be raised?
   (equal (multiple-value-list (unpack "c2N" "ABCDEF")) '(#x41 #x42 #x43444546)) ;; pick up after repeater
   ))

(deftest unpack-strings () 
  (check
    (string= (unpack "a*" "Test String") "Test String") ;; * repeater with string data
    (string= (unpack "a5" (concatenate 'string "1234" (string #\null))) (concatenate 'string "1234" (string #\null))) ;; test null of 'a'
    (string= (unpack "A10" "Test      ") "Test");; numeric repeater with string data and padding
    (equal (multiple-value-list (unpack "A*N" "String ABCD  ")) '("String ABCD")) ;; it doesn't pick up after string/*
    (equal (multiple-value-list (unpack "A10V" "Test      DCBA")) '("Test" #x41424344)) ;; pick up after string/#
    (equal (multiple-value-list (unpack "c2xa*" (concatenate 'string "AB" (string #\null) "Test"))) '(#x41 #x42 "Test")) ;; non consuming 'x' plays nicely with others

    (string= (unpack "B*" "ABC") "010000010100001001000011") ;; binary string
    (string= (unpack "B15" "ABC") "010000010100001") ;; binary string only consumes what's asked of it
    (string= (unpack "b*" "AB") "1000001001000010") ;; other ordering
    (string= (unpack "B9" "A") "01000001") ;; Not enough data
    (string= (unpack "H3" "AB") "414") ;; basic hex string (ignoring extra chars)
    (string= (unpack "H*" "A@") "4140") ;; padding right?
    (string= (unpack "h*" "AB") "1424") ;; other byte ordering
    (string= (unpack "H3" "A") "41") ;; not enough data

    (string= (unpack "Z*" (concatenate 'string "dan" (string #\null))) (concatenate 'string "dan" (string #\null))) ;; null padded string *
    ))


;; Apparently in perl land 1 byte and 2 byte values overflow and wrap arround 
;; but 4 byte values don't.  This is inconsistent and hard to match perfectly.
;; So cl-pack prevents all overflow and underflow pegging numbers at their
;; highest or lowest possible value as per perl's 4 byte behaviour
(deftest pack-signed ()
  (check 
    (string= (unpack "B*" (pack "c" 255)) "11111111")
    (string= (unpack "B*" (pack "c" 256)) "11111111")
    (string= (unpack "B*" (pack "c" -128)) "10000000")
    (string= (unpack "B*" (pack "c" -129)) "10000000")))
   

(deftest unpack-signed ()
  (check 
    (= (unpack "c" (pack "c" -1)) -1)
    (= (unpack "c" (pack "c" -129)) -128)
    (= (unpack "c" (pack "c" 127)) 127)
    (= (unpack "c" (pack "c" 128)) -128)))


;;; Test that weird things in form are still handled ok

(deftest pack-form ()
  (check
    (string= (pack "Kc" #x41 #x42) "A"))) ; unknown character

(deftest unpack-form ()
  (check
    (= (unpack "Kc" "AB") #x41))) ; unknown character

(deftest mod-! ()
  (check
    (= (unpack "n!" (pack "n" -1)) -1) ; n! is signed
    ))

(deftest mod-<> ()
  (check 
    (string= (pack "l>" #x41424344) "ABCD")
    (string= (pack "l<" #x41424344) "DCBA")
    (= (unpack "l<" (pack "V" #x41424344)) #x41424344)
    (= (unpack "l>" (pack "N" #x41424344)) #x41424344)
    ))
