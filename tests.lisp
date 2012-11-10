;;;; ******************************************************
;;;; FILE IDENTIFICATION
;;;; 
;;;; Name:     tests.lisp
;;;; Purpose:  Tests for CL-PACK
;;;; Author:   Dan Ballard <http://mindstab.net>
;;;; Created:  May 2009
;;;; Modified: August 2009
;;;; License:  BSD
;;;; Description:  CL-PACK supplies perl/php/ruby compatible
;;;;               pack() and unpack() functions to allow
;;;;               easy use of binary protocols with the above
;;;;               mentioned languages and C.
;;;;*******************************************************

(in-package :common-lisp)

(defpackage :cl-pack-test
  (:use #:common-lisp #:cl-pack)
  (:export #:test #:test-silent))

(in-package :cl-pack-test)

;;;; ***** Unit Test Framework shamelessly ripped from *****
;;;;       Practical Common Lisp (and slightly modified)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defvar *test-name* nil)
(defvar *silent* nil)

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
       (if (not *silent*) (format t "~a ~d/~d passed~%" *test-name* ,pass-count ,total-count))
       (values ,result ,pass-count ,total-count))))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (if (not *silent*) (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form))
  result)

;;;; ********************** Unit Tests ****************************


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
   (mod-<>)
   (grouping)
   (pack-/)
   (unpack-/)
   ))

(deftest test-silent ()
    (let ((*silent* t))
      (test)))

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
   (string= (pack "w" 0) (coerce `(,(code-char 0)) 'string))
 ))


(deftest pack-combinations ()
  (check
   (string= (pack "c2" #x41 #x42 #x43) "AB") ; basic repeater (with extra data dropped)
   (string= (pack "c*" #x41 #x42 #x43) "ABC") ; basic * repeater
   (string= (pack "c3" #x41 #x42) "AB") ; only use avail data -- ! should ERROR be raised?
   (string= (pack "c2N" #x41 #x42 #x43444546) "ABCDEF") ; pick up after repeater
   (string= (pack "NX2" #x41424344) "AB") ;delete chars
   (string= (pack "c.c" 65 0 66 ) "B") ; truncate
   (string= (pack "c.c" 65 2 66) (concatenate 'string "A" (string #\null) "B")) ; null pad
   (string= (pack "c@0c" 65 66) "B") ;truncate
   (string= (pack "c@2c" 65 66) (concatenate 'string "A" (string #\null) "B")) ; null pad
   ))

(deftest pack-strings () 
  (check
    (string= (pack "a*" "Test String") "Test String") ; * repeater with string data
    (string= (pack "a5" "1234") (concatenate 'string "1234" (string #\null))) ; test null of 'a'
    (string= (pack "a*" 65) "65") ; pack a string converting a number
    (string= (pack "A10" "Test") "Test      ") ; numeric repeater with string data and padding
    (string= (pack "a*N" "String" #x41424344) "StringABCD")  ; pick up after string/*
    (string= (pack "A10V" "Test" #x41424344) "Test      DCBA") ; pick up after string/#
    (string= (pack "c2xa*" #x41 #x42 "Test") (concatenate 'string "AB" (string #\null) "Test")) ; non consuming 'x' plays nicely with others
    (string= (pack "B*" "010000010100001001000011") "ABC") ; binary string
    (string= (pack "B16" "010000010100001001000011") "AB") ; binary string only consumes what's asked of it
    (string= (pack "B*" "010000010100001") "AB") ; AB string short a bit
    (string= (pack "b*" "100000101000010") "A!") ; AB string short a bit
    (string= (pack "H4" "414243") "AB") ; basic hex string (ignoring extra chars)
    (string= (pack "H*" "414") "A@") ; padding right?
    (string= (pack "h*" "1424") "AB") ; other byte ordering
    (string= (pack "Z*" "dan") (concatenate 'string "dan" (string #\null))) ; null padded string *
    (string= (pack "Z5" "dan") (concatenate 'string "dan" (string #\null) (string #\null))) ; padding of Z
    (string= (pack "Z3" "dan") (concatenate 'string "da" (string #\null))) ; proper ending in NULL for under length string

    
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
   (= (unpack "w" (coerce `(,(code-char 0)) 'string)) 0)
   ))

(deftest unpack-combinations ()
  (check
   (equal (multiple-value-list (unpack "c2" "ABC")) '(#x41 #x42)) ; basic repeater (with extra data dropped)
   (equal (multiple-value-list (unpack "c*" "ABC")) '(#x41 #x42 #x43)) ; basic * repeater
   (equal (multiple-value-list (unpack "c3" "AB")) '(#x41 #x42)) ; only use avail data -- ! should ERROR be raised?
   (equal (multiple-value-list (unpack "c2N" "ABCDEF")) '(#x41 #x42 #x43444546)) ; pick up after repeater
   ))

(deftest unpack-strings () 
  (check
    (string= (unpack "a*" "Test String") "Test String") ; * repeater with string data
    (string= (unpack "a5" (concatenate 'string "1234" (string #\null))) (concatenate 'string "1234" (string #\null))) ; test null of 'a'
    (string= (unpack "A10" "Test      ") "Test"); numeric repeater with string data and padding
    (equal (multiple-value-list (unpack "A*N" "String ABCD  ")) '("String ABCD")) ; it doesn't pick up after string/*
    (equal (multiple-value-list (unpack "A10V" "Test      DCBA")) '("Test" #x41424344)) ; pick up after string/#
    (equal (multiple-value-list (unpack "c2xa*" (concatenate 'string "AB" (string #\null) "Test"))) '(#x41 #x42 "Test")) ; non consuming 'x' plays nicely with others

    (string= (unpack "B*" "ABC") "010000010100001001000011") ; binary string
    (string= (unpack "B15" "ABC") "010000010100001") ; binary string only consumes what's asked of it
    (string= (unpack "b*" "AB") "1000001001000010") ; other ordering
    (string= (unpack "B9" "A") "01000001") ; Not enough data
    (string= (unpack "H3" "AB") "414") ; basic hex string (ignoring extra chars)
    (string= (unpack "H*" "A@") "4140") ; padding right?
    (string= (unpack "h*" "AB") "1424") ; other byte ordering
    (string= (unpack "H3" "A") "41") ; not enough data

    (string= (unpack "Z*" (concatenate 'string "dan" (string #\null))) (concatenate 'string "dan" (string #\null))) ; null padded string *
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

;; Test both < > mod features and that they play nice with repeaters
(deftest mod-<> ()
  (check 
    (string= (pack "l>" #x41424344 #x45464748) "ABCD")
    (string= (pack "l<*" #x41424344 #x45464748) "DCBAHGFE")
    (string= (pack "s<" #x4142) "BA") ; check s
    (string= (pack "s>" #x4142) "AB") ; check s
    (= (unpack "l<" (pack "V" #x41424344)) #x41424344)
    (equal (multiple-value-list (unpack "l>2" (pack "N*" #x41424344 #x45464748))) '(#x41424344 #x45464748))
    (= (unpack "s<" "AB") #x4241) ;test s
    (= (unpack "s>" "AB") #x4142) ; test s
    ))

(deftest grouping ()
  (check
    (string= (pack "(ccx(cx)2)2" #x41 #x42 #x43 #x44 #x45 #x46 #x47 #x48) 
	     (concatenate 'string "AB" (string #\null) "C" (string #\null) "D" (string #\null) "EF" (string #\null) "G" (string #\null) "H" (string #\null))) ; Complex nested grouping with non consuming elements
    (string= (pack "(ccx(cx)2)2" #x41 #x42 #x43 #x44 #x45 #x46 #x47) 
	     (concatenate 'string "AB" (string #\null) "C" (string #\null) "D" (string #\null) "EF" (string #\null) "G" (string #\null))) ; same as above except missing a data item

    (equal (multiple-value-list (unpack "(cc(c)2)2" "ABCDEFGH")) 
	   '((65 66 (67) (68)) (69 70 (71) (72)))) ;unpack complex nested group
    (equal (multiple-value-list (unpack "(ccx(cx)2)2" "ABCDEFGHIJKLMNOP")) 
	   '((65 66 (68) (70)) (72 73 (75) (77)))) ;unpack complex nested group with skips
    (equal (multiple-value-list (unpack "(ccx(cx)2)2" "ABCDEFGH")) 
	   '((65 66 (68) (70)) (72))) ;unpack complex nested group with skips missing elements

    (string= (pack "(s(ss>s)<s)>" #x4142 #x4344 #x4546 #x4748 #x494a) "ABDCEFHGIJ") ; modifiers work over whole groups, and locality of modifier precidence
    (equal (multiple-value-list (unpack "(s(ss>s)<s)>" "ABCDEFGHIJ"))
	   '((#x4142 (#x4443 #x4546 #x4847) #x494a)))

    ))

(deftest pack-/ ()
  (check
    (string= (pack "a/c3" 65 66) "2AB") ; basic pack (missing an element, into a string type
    (string= (pack "n/c*" 65 66 67) (concatenate 'string (string #\null) (string (code-char 3)) "ABC")) ;basic pack into a number with a *
    (string= (pack "a/a3" "ABC") "3ABC") ;string pack
    (string= (pack "a/a*" "ABC") "3ABC") ;string pack with *
    ))

(deftest unpack-/ ()
  (check
    (equal (multiple-value-list (unpack "a/c" "3AB")) '(65 66))
    (equal (multiple-value-list (unpack "n/c" (concatenate 'string (string #\null) (string (code-char 3)) "ABC"))) '(65 66 67))
    (equal (multiple-value-list (unpack "a/ac" "2ABC")) '("AB" 67))
  
    ))