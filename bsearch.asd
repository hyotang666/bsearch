; vim: ft=lisp et
(in-package :asdf)
(defsystem "bsearch"
  :depends-on
  nil
  :license "Public Domain"
  :author "Shinichi Sato"
  :description "Just only binary searching."
  :components
  ((:file "bsearch")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "bsearch"))))
  (append (call-next-method)'((test-op "bsearch.test"))))
