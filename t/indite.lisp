(in-package :cl-user)
(defpackage :rosa-test.indite
  (:use :cl
        :rosa
        :prove))
(in-package :rosa-test.indite)


(subtest "nil maps to empty string"
  (is (indite '()) "")
  (is (indite (make-hash-table)) ""))
