(defpackage #:bsearch
  (:use #:common-lisp)
  (:export
    #:bsearch
    ))
(in-package #:bsearch)

(declaim(ftype (function (t vector
			    &key
			    (:key (or function symbol))
			    (:test (or function symbol))
			    (:start (mod #.most-positive-fixnum))
			    (:end (mod #.most-positive-fixnum))
			    (:compare (or function symbol))
			    (:default t))
			 t)
	       bsearch))

(defun bsearch(item vector &key (key #'identity)(test #'eql)(start 0)(end (length vector)) (compare #'<)(default nil))
  (%bsearch item vector
	    (coerce key 'function)
	    (coerce test 'function)
	    start end
	    (coerce compare 'function)
	    default))

(declaim(ftype (function (t vector function function
			    (mod #.most-positive-fixnum)
			    (mod #.most-positive-fixnum)
			    function t)
			 t)
	       %bsearch))

(defun %bsearch(item vector key test start end compare default)
  (declare (type (mod #.most-positive-fixnum) start end)
	   (dynamic-extent key test compare)
	   (optimize(speed 3)(safety 0)(debug 0)))
  (assert(< -1 start end (1+(length vector))))
  (prog((center 0)
	(% 0)
	(index 0)
	target elt)
    (declare (type fixnum center % index))
    REC
    (multiple-value-setq(center %)(floor(- end start)2))
    (setq index (+ center start (if(zerop(+ center %))
				  1
				  0))
	  target (aref vector index)
	  elt (funcall key target))
    (cond
      ((funcall test item elt)
       (return (values target index)))
      ((= 1 (+ center %))
       (return (values default nil)))
      ((funcall compare item elt)
       (setf end index))
      (t(setf start index)))
    (go REC)))

(define-compiler-macro bsearch(item vector &key key test start end compare default)
  (let((v(gensym "VECTOR")))
    `(let((,v ,vector))
       (declare(type simple-vector ,v))
       (%bsearch ,item
		 ,v
		 ,(or (and key `(coerce ,key 'function))
		      '#'identity)
		 ,(or (and test `(coerce ,test 'function))
		      '#'eql)
		 ,(or start 0)
		 ,(or end `(length ,v))
		 ,(or (and compare `(coerce ,compare 'function))
		      '#'<)
		 ,default))))
