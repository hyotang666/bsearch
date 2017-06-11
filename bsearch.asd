; vim: ft=lisp et
(in-package :asdf)
(defsystem "bsearch"
  :depends-on
  nil
  :components
  ((:file "bsearch")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "bsearch"))))
  (test-system :bsearch.test))
