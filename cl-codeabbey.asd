(defsystem "cl-codeabbey"
  :version "0.1.0"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("str")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Solving CodeAbbey problems in Common Lisp"
  :in-order-to ((test-op (test-op "cl-codeabbey/tests"))))

(defsystem "cl-codeabbey/tests"
  :author "Hamza Shahid"
  :license "BSD 2 Clause License"
  :depends-on ("cl-codeabbey"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-codeabbey"
  :perform (test-op (op c) (symbol-call :rove :run c)))
