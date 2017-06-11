; vim: ft=lisp et
(in-package :asdf)
(defsystem :bsearch.test
  :depends-on
  (:jingoh "bsearch")
  :components
  ((:file "bsearch"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :bsearch)))