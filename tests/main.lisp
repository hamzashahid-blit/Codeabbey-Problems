(defpackage cl-codeabbey/tests/main
  (:use :cl
        :cl-codeabbey
        :rove))
(in-package :cl-codeabbey/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-codeabbey)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
