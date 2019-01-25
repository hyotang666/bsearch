#++(dev:load :bsearch)
(in-package :bsearch)

(defun vec(size)
  (make-array size :initial-contents (loop :for i :below size :collect (code-char i))))

(defun bench(vector)
  (let((size(length vector)))
    (print :find)
    (time(dotimes(x 10000)
	   (find (code-char (random size))vector )))
    (print :bsearch)
    (time(dotimes(x 10000)
	   (bsearch (code-char(random size))vector :compare #'char<)))
    (print :bsearch2)
    (time(dotimes(x 10000)
	   (bsearch2 (code-char(random size))vector :compare #'char<)))
    (print :bsearch3)
    (time(dotimes(x 10000)
	   (bsearch3 (code-char(random size))vector :compare #'char<)))))

(bench 5000)
