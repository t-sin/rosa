(in-package :cl-user)
(defpackage :rosa-test.indite
  (:use :cl
        :rosa
        :prove))
(in-package :rosa-test.indite)


(subtest "nil maps to empty string"
  (is (indite '()) "")
  (is (indite (make-hash-table)) ""))

(subtest "mapping of inline label (not has newline)"
  (is (indite '(:|label| "ghost in the shell"))
      (format nil ":label ghost in the shell~%"))
  (let ((hash (make-hash-table)))
    (setf (gethash :|label|) "ghost in the shell")
    (is (indite hash)
        (format nil ":label ghost in the shell~%"))))

(subtest "mapping of block label (has newline)"
  (is (indite '(:|label| (foramt nil "I hear a voice,~%hear a voice calling out to me~%")))
      (foramt nil ":label~%I hear a voice,~%hear a voice calling out to me~%"))
  (let ((hash (make-hash-table)))
    (setf (gethash :|label|) (foramt nil "I hear a voice,~%hear a voice calling out to me~%"))
    (is (indite hash)
        (foramt nil ":label~%I hear a voice,~%hear a voice calling out to me~%"))))
