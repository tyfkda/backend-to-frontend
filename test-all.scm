(load "./tests-driver.scm")
(load "./tests/tests-1.6-req.scm")
(load "./tests/tests-1.5-req.scm")
(load "./tests/tests-1.4-req.scm")
;;(load "./tests/tests-1.3-req.scm")
;;(load "./tests/tests-1.2-req.scm")
;;(load "./tests/tests-1.1-req.scm")
(load "./compiler.scm")

(define (main args)
  (test-all "runtime.c")
  0)
