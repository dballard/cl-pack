;;;; ******************************************************
;;;; FILE IDENTIFICATION
;;;; 
;;;; Name:     cl-pack.lisp
;;;; Purpose:  CL-PACK code
;;;; Author:   Dan Ballard <http://mindstab.net>
;;;; Created:  May 2009
;;;; Modified: August 2009
;;;; License:  BSD
;;;; Description:  CL-PACK supplies perl/php/ruby compatible
;;;;               pack() and unpack() functions to allow
;;;;               easy use of binary protocols with the above
;;;;               mentioned languages and C.
;;;;*******************************************************

(in-package #:cl-pack)


;;; ************* DOCUMENTATION AND NOTES **************
;;; mostly from http://perldoc.perl.org/functions/pack.html

;;; Characters accpeted in FORM by pack and unpack
;;; as defined by the perl documentation for pack()
;;; and unpack()

;;; **** SUPPORTED TEMPLATE COMMANDS****

;;;a A string with arbitrary binary data, null padded
;;;A A text (ASCII) string, space padded
;;;Z A null termnated (ASCII) string, null paddded

;;;b A bit string (ascending bit order inside each byte, like vec()).
;;;B A bit string (descending bit order inside each byte).
;;;
;;;h A hex string (low nybble first).
;;;H A hex string (high nybble first).

;;;c signed char 8 bit
;;;C unsigned char (octet)

;;;s signed short 16bit
;;;S unsigned short 16bit
;;;l signed long 32bit
;;;L unsighed long 32bit
;;;q signed quad 
;;;Q unsigned quad
;;;i signed integer (at least 32, depends on what compiler calls 'int')
;;;I unsigned integer (machine dependant size & order)

;;;f single precision float
;;;d double precision float

;;;x null byte
;;;X Backup a byte (pack only)

;;; (pack only)
;;;@   Null fill or truncate to absolute position specified by repeater
;;;.   Null fill or truncate to absolute position specified by value/argument

;;;n unsighed short (16bit big endian)
;;;v unsigned short (16bit little endian)
;;;N unsigned long (32bit big endian)
;;;V unsigned long (32bit little endian)

;;;w	A BER compressed integer (not an ASN.1 BER, see perlpacktut for
;;;	details).  Its bytes represent an unsigned integer in base 128,
;;;	most significant digit first, with as few digits as possible.  Bit
;;;	eight (the high bit) is set on each byte except the last.


;;; ***** FROM RUBY ******
;;;e single precision float (little endian)
;;;g single precision float (big endian)
;;;E double precision float (little endian)
;;;G double precision float (big endian)

;;; ***** MODIFIERS ******
;;;  #  form#  repeats the form operation # times
;;;  *  form*  repeats the form operation on all available arguments

;;;  !   nNvV  Treat as signed integer instead of unsigned 
;;;  >   sSiIlLqQfd  Force big endian
;;;  <   sSiIlLqQfd  Force little endian

;;; ***** GROUPING *****
;;; ()   Example: (pack "(cc)3" 65 66 67 68 69 70) => "ABCDEF"
;;;      Example: (unpack "(cc)3") "ABCDEF") => (65 66) (67 68) (69 70)

;;; ***** / Template *****
;;; sequence length / sequence item
;;;   in pack, writes out how ever many sequence items PRECEDED by the length 
;;;   in the form of sequence length
;;;   example: (pack "a/c3" 65 66 67) => "3ABC"
;;;   in unpack, reads the length and unpacks that many
;;;   example: (unpack "a/c" "3ABC") => (65 66 67)

;;; **** NOTE *****

;;; A lot use users of pack() and unpack() in other languages
;;; split 64 bit values into two longs and send them as
;;; N2 or NN
;;; because there is no endian safe handling of 64 bit quads
;;; specified
;;; in cl-pack you can also use q< , q> , Q< and Q>


;;; ***************** CL-PACK **********************


;;; Determine as best we can the endian-ness of the host system
;;; for all the function that map to the host endian-ness
#+(or x86 x86-64)(push :little-endian *features*)
#+(or sparc powerpc ppc) (push :big-endian *features*)
;;;BI? what to do with: (alpha arm)?
;;; If we dont have an endian yet, we need one, so just default to one and hope
;; has to be done as find, not - feature because that's done at compile time 
;; and we need this to be done at run time
(if (not (or (find :little-endian *features*) (find :big-endian *features*)))
    (push :big-endian *features*))

;;; The int encoding maps to host size of integer so try to determine that
#+(or x86-64 64bit)(push :long-integer *features*)


;;; **** Utils ****
(defun strhead (str)
  "returns a char that is the first char of str"
  (char str 0))

(defun strtail (str)
  "returns the rest of str"
  (subseq str 1))

(defmacro inc-form ()
  "create a subseq of form that skips the current syntax object"
  `(setf new-form (subseq form offset)))


;;; **** Basic byte conversion stuff ****

(defun twos-complement (number max-size)
  (if (< number 0) ;;(> number (expt 2 (1- max-size)))
      (1+ (lognot (min (expt 2 (1- (* 8 max-size))) (abs number))))
      (min (1- (expt 2 (* 8 max-size))) number)))

(defun un-twos-complement (number max-size)
  (if (>= number (expt 2 (1- (* 8 max-size))))
      (- number (expt 2 (* 8 max-size)))
      (min (expt 2 (1- (* 8 max-size))) number)))

(defun ber-encode (number)
  "function to encode a BER number into a binary byte string"
  (let ((num_bytes (ceiling (/ (log (1+ number) 2) 7)))
	(n number))
    (if (eql 0 n) (incf num_bytes))
    (coerce (loop for i from  (1- num_bytes) downto 0 collect 
	 (code-char (+ (if (> i 0)
			   128
			   0)
		       (let ((base (* i 7)))
			 (loop for j from 6 downto 0 sum
			      (let ((exp (expt 2 (+ base j))))
				(if (>= n exp)
				    (progn 
				      (decf n exp)
				      (expt 2 j))
				    0)))))))
	    'string)))

(defun ber-decode (string)
  "Take a BER number as a binary string and returns a number"
  (loop for i from 0 to (1- (length string)) sum
       (* (expt 2 (* 7 (- (length string) 1 i)))
	  (- (char-code (char string i))
	     (if (< i (1- (length string)))
		 128 
		 0)))))

(defun ber-str-length (string)
  (if (>= (char-code (char string 0)) 128)
      (1+ (ber-str-length (strtail string)))
      1))
	    

(defun bytes-to-list (bytes length)
  "bytes:  Some binary data in lisp number form that ldb can access
  bytes-to-list pulls out 8bit bytes from bytes and turns them into
    their corresponding characters and returns the list of them"
  (loop for i from (- length 1) downto 0 collect (code-char (ldb (byte 8 (* 8 i)) bytes))))
 

;;; BIG ENDIAN
(defun bytes-to-string (bytes length)
  "puts length bytes from bytes into a string"
  (coerce (bytes-to-list bytes length) 'string))

;;; LITTLE ENDIAN
(defun bytes-to-string-rev (bytes length)
  "puts length bytes from bytes into a reversed string"
  (coerce (reverse (bytes-to-list bytes length)) 'string))


(defun unpack-bytes (string length)
  "takes length bytes from string and returns an int"
  (let ((int 0))
    (loop for i from 0 to (1- length) do 
	 (setf (ldb (byte 8 (* 8  i)) int)  (char-code (char string i))))
    int))

;;; BIG ENDIAN
(defun string-to-bytes (string length)
  (unpack-bytes (reverse string) length))


;;; LITTLE ENDIAN
(defun string-to-bytes-rev (string length)
  (unpack-bytes string length))


;;; **** String data stuff ****

(defmacro handle-string ((repeater repeater-star) star-body count-body else-body)
  "macro for building string type bodies for case statements in pack() or unpack()"
  `(if ,repeater-star
		 (progn
		   ;(setf ,new-form (subseq ,new-form 2))
		   (inc-form)
		   ,star-body)
		 (if (> ,repeater 0) ;; no *, a #?
		     (let ((result (progn ,count-body)))
		       ;(setf new-form (subseq form (1+ ,repeater-chars)))
		       (inc-form)
		       result)
		     (progn ,else-body)) ;; no repeater #
		 ))

(defmacro pack-string ((repeater repeater-star) star-body count-body else-body)
  "macro for building string type bodies for case statements in pack()"
 `(progn
    (if (numberp item)
	(setf item (write-to-string item)))
    (handle-string (,repeater ,repeater-star) ,star-body ,count-body ,else-body)))

(defmacro unpack-string ((repeater repeater-star) star-body count-body else-body)
  "macro for building string type bodies for case statements in unpack()"
  `(handle-string (,repeater ,repeater-star) ,star-body ,count-body ,else-body))


(defun 8bits-to-byte (8bits &optional (byte-form (lambda (i) (byte 8 (- 7 i)))))
  "turns a string of 8 or less bits into a byte
     byte-form specifies the packing order of bits into the byte, deaulting to decending order"
  (let ((byte 0))
    (loop for i from 0 to (min 7 (1- (length 8bits))) 
       do (if (char= (char 8bits i) #\1)
	      (incf byte (dpb 1 (funcall byte-form i) 0))))
    (code-char byte)))

(defun byte-to-8bits (byte)
  "turns a byte into a string of bits"
  (format nil "~8,'0B" byte))

(defun byte-to-8bits-rev (byte)
  "convert a byte to a bit string, lowest bit first"
  (reverse (byte-to-8bits byte)))


(defun bit-pack (bit-str &optional (byte-form (lambda (i) (byte 8 (- 7 i)))))
  "pack a bit string into a byte string, decending order by default"
 (coerce (loop for i from 0 to (1- (length bit-str)) by 8 collecting (8bits-to-byte (subseq bit-str i (min (length bit-str) (+ i 8))) byte-form)) 'string))

(defun bit-unpack (byte-str &optional (unpack-fn #'byte-to-8bits))
  "turn a string of bytes into an extended string of bits unpacked by unpack-fn"
  (apply #'concatenate 'string 
	 (loop for i from 0 to (1- (length byte-str)) collecting 
	      (funcall unpack-fn (char-code (char byte-str i))))))

(defun hex-to-number (hex)
  "turn a character of 0-9 or a-f or A-F into a hex digit of 0-15"
  (digit-char-p hex 16))
      
;;; Not needed any more
;;(defun number-to-hex (num)
;;  "convert a number 0-15 to a hex character"
;;  (digit-char num 16))

(defun 2hex-to-byte (2hex &optional (mapper (lambda (2hex) (values (char 2hex 0) (char 2hex 1)))))
  "Turn a 2 hex digit string into a number unpacked by mapper"
  (multiple-value-bind (a b) (funcall mapper 2hex)
    (+ (* 16 (hex-to-number (coerce a 'character))) (hex-to-number (coerce b 'character)))))
    
(defun byte-to-2hex (byte)
  "Turn a byte into a string of 2 hex characters"
  (format nil "~2,'0X" byte))
  
(defun byte-to-2hex-rev (byte)
  (reverse (byte-to-2hex byte)))

(defun hex-pack (hex-str &optional (mapper (lambda (2hex) (values (char 2hex 0) (char 2hex 1)))))
  "turn a string of hex digits into a string of packed bytes, unpacking
   2 hex digits at a time by mapper"
  (let ((str (if (= (mod (length hex-str) 2) 1)
		 (concatenate 'string hex-str "0" )
		 hex-str)))
    (coerce (loop for i from  0 to (1- (length str)) by 2 collecting (code-char (2hex-to-byte (subseq str i (+ i 2)) mapper))) 'string)))

(defun hex-unpack (byte-str &optional (hex-unpack-fn #'byte-to-2hex))
  "Turn a string of bytes into a string of hex digits"
  (apply #'concatenate 'string (loop for i from 0 to (1- (length byte-str)) collecting (funcall hex-unpack-fn (char-code (char byte-str i))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun endian-type-to-func (endian)
    (if (eql endian :big) '#'bytes-to-string
	(if (eql endian :native)
	    'bytes-to-string-fn
	    '#'bytes-to-string-rev))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun un-endian-type-to-func (endian)
    (if (eql endian :big) '#'string-to-bytes
	(if (eql endian :native)
	    'string-to-bytes-fn
	    '#'string-to-bytes-rev))))


(defmacro pack-int (size endian)
  "Macro to define the pack function for an int"
  (let ((pack-fn (endian-type-to-func endian)))
    `(funcall ,pack-fn (twos-complement item ,size) ,size)))

(defmacro unpack-int (size endian)
  "Macro to define the unpack function for a signed int"
  (let ((unpack-fn (un-endian-type-to-func endian)))
    `(un-twos-complement (funcall ,unpack-fn (cut-str string ,size new-str) ,size) ,size)))

(defmacro unpack-uint (size endian)
  "Macro to define the unpack function for an unsigned int"
  (let ((unpack-fn (un-endian-type-to-func endian)))
    `(funcall ,unpack-fn (cut-str string ,size new-str) ,size)))

(defmacro unpack-mod!-uint (size endian)
  "macro to define a normal uint that with the ! modifier is a signed int"
  `(if mod-! 
      (unpack-int ,size ,endian)
      (unpack-uint ,size ,endian)))


(defun next-char (form offset)
  "Get the next char from a string of null if offset is past end of string"
  (if (>= offset (length form)) 
      #\null 
      (char form offset)))


(defun find-matching-paren (str)
  ;; takes a string returns the offset of the closing parenthesis, return -1 on fail
  (let ((depth 0))
    (do ((i 0 (incf i)))
      ((or (< depth 0) (>= i (length str))) (if (< depth 0) (1- i) -1))
      (if (char= (char str i) #\()
	  (incf depth)
	  (if (char= (char str i) #\))
	      (decf depth))))))



;;; The header of a function (pack or unpack) that parses a form as defined above
;;; parses form and generates variables
;;;   repeater, repeater-star repeater-chars new-form 
;;; and then executes body
(defmacro def-form-parser (fn-name (&rest extra-args) end-test final-item &rest body)
  `(defun ,fn-name (form ,@extra-args)
     ;(format t "parser: form:'~a'~%" form)

     ;; if termination tests, return final item
     (if (or (string= form "") ,end-test) 
	 ,final-item
	 
	 ;; parsing variables and init
	 (let ((offset 1)
	       (inner-length 0)
	       (repeater-star nil)
	       (repeater nil)
	       (mod-! nil)
	       (mod-> nil)
	       (mod-< nil)
	       (/-pos 0))

	   (if (char= #\( (char form 0))
	       (progn 
		 (setf inner-length (find-matching-paren (strtail form)))
		 (if (= inner-length (- 1))
		     (error "cl-pack: Syntax error in 'form': unmatched bracket at '~a'~%" form)
		     (setf offset (+ 2 inner-length)))))

	   ;; parse repeaters and modifiers
	   (do ((str (subseq form offset) (subseq form offset))
		(offset1 offset offset)
		(offset2 0 offset1))
	       
	       ((= offset offset2))
	     ;; try to get a number and how long it is from form
	     (multiple-value-bind (repeater-count repeater-chars) 
		 (if (>= (length str) 1)
		     (parse-integer str :junk-allowed t) 
		     (values nil 0))
	       (if (and (>= (length str) 1) (eql repeater-count nil))
		   ;; no repeater #, check for other modifiers ( * ! < > )
		   (case (char str 0)
		     (#\! (progn (setf mod-! t) (incf offset)))
		     (#\> (progn (setf mod-> t) (incf offset)))
		     (#\< (progn (setf mod-< t) (incf offset)))
		     (#\* (progn (setf repeater-star t) (incf offset)))
		     (#\/ (progn 
			    (setf /-pos offset)
			    ;; a/N... we need offset to point to after N...
			    ;; so we need to parse it so inc offset to after 'N'
			    (incf offset 2) 
			    ))
		     )
		   
		   (progn ; repeater-count == #
		     (if repeater-count (setf repeater repeater-count))
		     (incf offset repeater-chars))
		   )))


	       (let ((new-form form))
		 (inc-form)

		 (if (or repeater-star (and repeater (> repeater 1)))
		     (setf new-form (concatenate 'string 
						 (subseq form 0 (if (> inner-length 0) (+ 2 inner-length) 1))
						 (if mod-! "!" "") 
						 (if mod-> ">" "")
						 (if mod-< "<" "")
						 (if repeater-star
						     "*"
						     (write-to-string (1- repeater)))
						 (subseq form offset))))

		 (progn
		   ,@body))))))


(defmacro gen-modifiers-list ()
  `(list (if mod-> :mod->) (if mod-< :mod-<) (if mod-! :mod-!)))

(defmacro set-modifiers (modifiers)
  `(loop for m in ,modifiers do 
			   (case m
			     ;; < > in modifiers are secondary to those already set in local form syntax
			     (:mod-> (if (not mod-<) (setf mod-> t)))
			     (:mod-< (if (not mod->) (setf mod-< t)))
			     (:mod-! (setf mod-! t)))))


;;; *********** The Main part ***********


;;;  pack
;;;  perl compatile pack() function.
;;;  form: is a string of characters corresponding to encodings of data
;;;  rest: the data to be 'packed'
;;;        there are two ways of calling pack with arguments
;;;          1) reguarly with a series of arguments
;;;          2) with a single list of the arguments.  
;;;          Pack will remove each argument it uses destructively from this list
;;;  returns: a string of 'packed' data
(def-form-parser pack (&rest rest)

  ;; extra end test    
  (and (eql nil rest) (and (not (eql (strhead form) #\x)) (not (eql (strhead form) #\X))))
  
  ;; result
  (if (and (>= (length rest) 2) (eql (first rest) :result))
      (second rest)
      "")

  ;;; BODY
  ;; Extra optional keyed parameters
  ;; :result      result is the result so far of the pack operation
  ;; :modifiers   modifiers are global modifiers such as those set on a 
  ;;                grouping like: (ss)<

 ; (format t "pack: form:'~a' rest:~a~%" form rest)
  
  (let ((result "")
	(modifiers nil))
    
    ;; parse extra optional keyed parameters
    (do ((end? nil)
	 (i 0))
	((or end? (>= i (length rest))) )
      (case (elt rest i)
	(:result (progn 
		   (setf result (second rest))
		   (setf rest (rest (rest rest)))))
	(:modifiers (progn
		      (setf modifiers (second rest))
		      (set-modifiers (second rest))
		      (setf rest (rest (rest rest)))))
	(otherwise (setf end? t))))
	 
    ;; Instead of passing a series of arguments, you can call it with a destructable list of arguments
    (let ((dlist-arg-style nil))
      (if (and (>= 1 (length rest)) (listp (first rest)))
	  (progn
	    (setf dlist-arg-style t)
	    (setf rest (first rest))
	    ))

      ;; second end test (redundant a little, can we merge?
    (if (and (or (eql nil rest) (equal '(nil) rest)) (and (not (eql (strhead form) #\x)) (not (eql (strhead form) #\X))))
	result
	(progn

	  ;; set up of required endian functions
    (let ((bytes-to-string-fn #'bytes-to-string-rev) ; LITTLE ENDIAN
	  (item (first rest))
	  (new-result nil)
	  (new-rest rest))

      #+big-endian(setf bytes-to-string-fn #'bytes-to-string) ; BIG ENDIAN
      (if mod-> 
	  (setf bytes-to-string-fn #'bytes-to-string))
      (if mod-< 
	  (setf bytes-to-string-fn #'bytes-to-string-rev))


      (if (or (not repeater) (> repeater 0))
	  (progn
		 (if (not repeater) (setf repeater 0))
		 (setf new-rest (rest rest)) ;consume here - default rest for numbers
      
      ;; pack case satement
	(setf new-result

	       ;; FORM of: sequence length / sequence items
	       (if (> /-pos 0)
		(progn ;; length item / sequence item
		  (let ((sequence-type (char form (1+ /-pos)))
			(rest-len (length rest))
			(consumed-length 0)
			(ret (pack (subseq form (1+ /-pos) offset ) :modifiers (gen-modifiers-list) rest)))

		    (setf consumed-length (- rest-len (if (eql nil (first rest)) 0 (length rest))))

		    ;; if its a string determine consumed length differently
		    (if (member sequence-type '(#\a #\A #\Z #\b #\B #\h #\H))
			(let ((item-length 
			       (if (numberp item)
				   (length (write-to-string item))
				   (length item))))
			  (setf consumed-length 
				(if repeater-star
				    item-length
				    (min repeater item-length)))))

		    (inc-form)
		    (concatenate 'string (pack (subseq form 0 /-pos) consumed-length) ret)))
		
		  ;; ALL other FORMS 
	    (case (strhead form)
	      (#\n		;Unsigned Short 16bit Big Endian AB=AB
	       (pack-int 2 :big))
	      (#\N	    ; Unsigned Long 32bit Big Endian ABCD=ABCD
	       (pack-int 4 :big))
	      (#\v	      ;Unsigned Short 16bit Litte Endian AB=BA
	       (pack-int 2 :little))
	      (#\V	 ; Unsigned Long 32bit Little Endian ABCD=DCBA
	       (pack-int 4 :little))
	      (#\g		   ; single precision float Bit Endian
	       (bytes-to-string (ieee-floats:encode-float32 (float item)) 4))
	      (#\G		   ; double precision float Bit Endian
	       (bytes-to-string (ieee-floats:encode-float64 (float item)) 8))
	      (#\e		; single precision float Little Endian
	       (bytes-to-string-rev (ieee-floats:encode-float32 (float item)) 4))
	      (#\E		; double precision float Little Endian
	       (bytes-to-string-rev (ieee-floats:encode-float64 (float item)) 8))
	      (#\w ; ~BER encoded number
	       (ber-encode item))
	      (#\c			;signed 8bit char
	       (pack-int 1 :big))
	      (#\C			;unsigned 8bit char
	       (pack-int 1 :big))
	      ;;(#\W ;wide char  ;; Wide chars in strings in concatenate seems to 
	      ;; crash :(
	      ;;	     (string (code-char item )))

	      ((#\s #\S)		;signed/unsigned short 16bit
	       (pack-int 2 :native))

	      ((#\l #\L)		;signed/unsigned short 32bit
	       (pack-int 4 :native))

	      ((#\q #\Q)		;signed/unsigned quad 64bit
	       (pack-int 8 :native))

	      ((#\i #\I)       		; signed/unsigned integer machine size
	       (let ((int-size 4))
		 #+long-integer(setf int-size 8)
		 (pack-int int-size :native)))
	    
	      (#\f			;single precision float
	       (funcall bytes-to-string-fn (ieee-floats:encode-float32 (float item)) 4))

	      (#\d			;double precision float
	       (funcall bytes-to-string-fn (ieee-floats:encode-float64 (float item)) 8))

	      ((#\a #\A) ;string with binary data, null padded/space padded
	       (pack-string (repeater repeater-star)
			    item
			    (concatenate 'string 
					 (subseq item 0 (min (length item) repeater))
					 (if (> repeater (length item))
					     (make-list (- repeater (length item)):initial-element (if (char= #\a (strhead form)) #\null #\space))
					     ""))
			    (string (char item 0))))
	      (#\Z ; null terminated /padded string
	       (pack-string (repeater repeater-star)
			    (concatenate 'string item (string #\null))
			    (concatenate 'string 
					 (subseq item 0 (min (length item) (1- repeater)))
					 (if (> (1- repeater) (length item))
					     (make-list (- repeater (length item)) :initial-element #\null)
					     (string #\null)))
			    (string #\null)))
			     
	      ((#\b #\B) ; bit strings
	       (let ((bit-mapper (if (char= #\b (strhead form))
				     (lambda (i) (byte 8 i)) ; ascending
				     (lambda (i) (byte 8 (- 7 i)))))) ;decending
		 (pack-string (repeater repeater-star)
			      (bit-pack item bit-mapper)
			      (bit-pack (subseq item 0 (min repeater (length item))) bit-mapper)
			      (bit-pack (subseq item 0 1) bit-mapper))))

	      ((#\h #\H) ; hex strings
	       (let ((byte-mapper (if (char= #\H (strhead form))
				      (lambda (2hex) (values (char 2hex 0) (char 2hex 1))) ;high nybble first
				      (lambda (2hex) (values (char 2hex 1) (char 2hex 0)))))) ; low nybble first
		 (pack-string  (repeater repeater-star) 
			       (hex-pack item byte-mapper)
			       (hex-pack (subseq item 0 (min repeater (length item))) byte-mapper)
			       (hex-pack (subseq item 0 1) byte-mapper))))
				 
	      (#\x ; null character
	       (progn
		 (setf new-rest rest) ; this function doesn't consume
		 (string #\null)))
		 
		(#\X ;backup a byte
	       (progn
		 (let ((delta 1))
		   (if repeater-star
		       (setf delta (length result))
		       (if (>= repeater 1)
			   (setf delta repeater)))
		(setf result (subseq result 0 (- (length result)
						 (min (length result) delta)))))
		 (inc-form)
		 (setf new-rest rest)
		""
		))

	      ((#\. #\@)
		      ;; . consume a numerical arg - null fill or truncate to that position
		      ;; @ null fill or truncate to repeater specified position
		 (let ((position item)) ; .
		   (if (char= #\@ (strhead form))
		       (progn
			 (setf position repeater)
			 (setf new-rest rest)
			 (inc-form)))
		   
		   (setf result (subseq result 0 (min (length result) position)))
		   (make-list (max 0 (- position (length result))) :initial-element #\null)
		 ))

		(#\( ; Grouping
		 (let ((ret
			(pack (subseq form 1 (1+ inner-length)) :modifiers (gen-modifiers-list) rest)))
			  (setf new-rest rest) ; for dlist-style carry on
			  ret))
		 		 
		(otherwise (progn
			   (setf new-rest rest) ; didn't do anything, don't consume anything
			   ""))))
	    ) ;; (setf new-result)

	  ;; if using a descructable arg list
	(if dlist-arg-style
	    (progn
	      (if (not (equal rest new-rest)) ; consumed an arg
		  (progn
		    ;; so remove the arg from the arg list
		    (setf (car rest) (second rest))
		    (setf (cdr rest) (rest (rest rest)))))
	      ;; regardless, continue using the dlist style
	      (setf new-rest (list rest))))))

    ;; Recursion for the rest of pack
    (apply #'pack (append (list new-form :result (concatenate 'string result new-result) :modifiers modifiers) new-rest))))))))

;; macro for unpack.
;; cuts out and returns part of a string to be used for processing while setting
;; new-str to the remainder
(defmacro cut-str (str len new-str)
  `(let ((ret (subseq ,str 0 ,len)))
     (setf ,new-str (subseq ,str ,len))
     (if consumed (incf consumed ,len))
     ret))
	
;;;  perl compatible unpack() function
;;;   form: a string of characters corresonding to decodings
;;;   string: a string of binary data to be decoded
;;;   consumed: optional key parameter.  If nil (default) nothing happens
;;;             if an integer, every byte consumed increments consumed
;;;             and it is the last value return in the values list
;;;  returns: the decoded data in specified format
(def-form-parser unpack (string &key (consumed nil) (modifiers nil))

  ;; extra end test
  (<= (length string) 0)

  ;; final item
  (if consumed 
      (values consumed nil)
      nil)

  (set-modifiers modifiers)

  ;; setting up of endian specific functions
  (let ((string-to-bytes-fn #'string-to-bytes-rev) ; LITTLE ENDIAN
	(new-str string))
    #+big-endian(setf string-to-bytes-fn #'string-to-bytes)
    (if mod-> 
	(setf string-to-bytes-fn #'string-to-bytes))
    (if mod-<
	(setf string-to-bytes-fn #'string-to-bytes-rev))

    ;; unpack case statement and recursive call to unpack
    ;; note: not tail optiomized :fix ?
    (apply #'values
	   (remove nil (append

		(if (or (not repeater) (> repeater 0))
   	          (progn
		    (if (not repeater) (setf repeater 0))

	      (if (> /-pos 0)
		(progn
		  (let* ((length-item (subseq form 0 /-pos))
			 (sequence-item (subseq form (1+ /-pos) offset))
			 (lengths (multiple-value-list (unpack length-item string :consumed 0 :modifiers modifiers)))
			 (seq-len (if (numberp (first lengths))
				      (write-to-string (first lengths))
				      (first lengths)))
			 
			 (ret (multiple-value-list (unpack (concatenate 'string sequence-item seq-len) (subseq string (second lengths)) :consumed 0 :modifiers modifiers)))
			 (consumed-len (+ (first (last ret)) (second lengths))))
		    (cut-str string consumed-len new-str)
		    (inc-form)
		    (nbutlast ret)
		 ))
	   (list					   
	    (case (strhead form)
	     (#\n (unpack-mod!-uint 2 :big)) ; unsigned short 16bit big endian
	     (#\N (unpack-mod!-uint 4 :big)) ; unsigned long 32bit big endian
	     (#\v (unpack-mod!-uint 2 :little)) ; unsigned short 16bit little endian
	     (#\V (unpack-mod!-uint 4 :little)) ; unsigned long 32bit little endian

	     (#\c (unpack-int 1 :big))	  ; 1 byte signed character 
	     (#\C (unpack-uint 1 :big))	  ; 1 byte unsigned character
	     (#\s (unpack-int 2 :native)) ; 2 byte signed native endian	       
	     (#\S (unpack-uint 2 :native)) ; 2 byte signed native endian
	     (#\l (unpack-int 4 :native)) ; 4 byte signed native endan	       
	     (#\L (unpack-uint 4 :native)) ; 4 byte unsigned native endian
	     (#\q (unpack-int 8 :natice)) ; 8 byte signed native endian
	     (#\Q (unpack-uint 8 :native)) ; 8 byte unsigned native endian
	     (#\i (let ((int-size 4)) ; native signed int size and endian
		    #+long-integer(setf int-size 8)
		    (unpack-int int-size :native)))

	     (#\I (let ((int-size 4)) ; native unsigned int size and endian
		    #+long-integer(setf int-size 8)
		    (unpack-int int-size :native)))
	       
	     (#\w (ber-decode (cut-str string (ber-str-length string)  new-str)))

	     (#\e   (ieee-floats:decode-float32 (string-to-bytes-rev (cut-str string 4 new-str) 4))) ; 4 byte floating point little endian
	     (#\E   (ieee-floats:decode-float64 (string-to-bytes-rev (cut-str string 8 new-str) 8))) ; 8 byte floating point little endian

	     (#\g   (ieee-floats:decode-float32 (string-to-bytes (cut-str string 4 new-str) 4))) ; 4 byte floating point big endian
	     (#\G   (ieee-floats:decode-float64 (string-to-bytes (cut-str string 8 new-str) 8))) ; 8 byte floating point big endian

	     (#\f   (ieee-floats:decode-float32 (funcall string-to-bytes-fn (cut-str string 4 new-str) 4))) ; 4 byte floating point native endian
	     (#\d   (ieee-floats:decode-float64 (funcall string-to-bytes-fn (cut-str string 8 new-str) 8))) ; 8 byte floating point native endian

	     ((#\a #\A #\Z)   ; chatacter string with various paddings
	      (let ((special-chars 
		     (if (char= #\A (strhead form)) 
			 (coerce '(#\null #\space) 'string)
			 "")))
			   
		(unpack-string (repeater repeater-star)
			     (string-trim special-chars (cut-str string (length string) new-str))
			     (string-trim special-chars (cut-str string (min repeater (length string)) new-str))
			     (cut-str string 1 new-str))))

	     ((#\b #\B)			; bit string
	      (let ((bit-unpack-fn (if (char= (strhead form) #\b) #'byte-to-8bits-rev #'byte-to-8bits)))
		(unpack-string (repeater repeater-star)
			     (bit-unpack (cut-str string (length string) new-str) bit-unpack-fn)
			     (subseq (bit-unpack (cut-str string (min (ceiling (/ repeater 8)) (length string)) new-str) bit-unpack-fn) 0 (min repeater (* 8 (length string))))
			     (subseq (bit-unpack (cut-str string 1 new-str) bit-unpack-fn) 0 1))))

	     ((#\h #\H)			; hex string
	      (let ((hex-unpack-fn (if (char= (strhead form) #\h) #'byte-to-2hex-rev #'byte-to-2hex)))
		(unpack-string (repeater repeater-star)
			     (hex-unpack (cut-str string (length string) new-str) hex-unpack-fn)
			     (subseq (hex-unpack (cut-str string (min (ceiling (/ repeater 2)) (length string)) new-str) hex-unpack-fn) 0 (min repeater (* 2 (length string))))
			     (subseq (hex-unpack (cut-str string 1 new-str) hex-unpack-fn) 0 1))))
	       
	     (#\x			; null character
	      (cut-str string 1 new-str)
	      nil)

	     (#\(			; grouping ()
	      (let* ((ret (multiple-value-list (unpack (subseq form 1 (1+ inner-length)) string :consumed 0 :modifiers (gen-modifiers-list))))
		       (sub-cons (first (last ret))))
		  (cut-str string sub-cons new-str)
		  (nbutlast ret))
	      )

	     (otherwise nil)
	     )))))

	   ;; result of recursion
	   (multiple-value-list (unpack new-form new-str :consumed consumed :modifiers modifiers))) :from-end t :count 1))))
