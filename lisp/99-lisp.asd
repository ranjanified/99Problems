(defsystem :99-lisp
  :version "0.0.1"
  :author "Nalin Ranjan"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "p01"))))
  :description "https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html"
  :in-order-to ((test-op (test-op "99-lisp/tests"))))

(defsystem :99-lisp/tests
  :author ""
  :license ""
  :depends-on (:99-lisp
               :fiveam)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for 99-lisp"
  :perform (test-op (op c) (symbol-call :fiveam :run-all-tests)))
