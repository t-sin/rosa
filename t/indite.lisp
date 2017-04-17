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
  (is (indite '(:|label| #("ghost in the shell")))
      (format nil ":label ghost in the shell~%"))
  (let ((hash (make-hash-table)))
    (setf (gethash :|label|) #("ghost in the shell"))
    (is (indite hash)
        (format nil ":label ghost in the shell~%"))))

(subtest "mapping of block label (has newline)"
  (is (indite `(:|label| #(,(foramt nil "I hear a voice,~%hear a voice calling out to me~%"))))
      (foramt nil ":label~%I hear a voice,~%hear a voice calling out to me~%"))
  (let ((hash (make-hash-table)))
    (setf (gethash :|label|) #((foramt nil "I hear a voice,~%hear a voice calling out to me~%")))
    (is (indite hash)
        (foramt nil ":label~%I hear a voice,~%hear a voice calling out to me~%"))))

(subtest "escape ':' and ';' in block"
  (is (indite `(:|label| #(,(format nil ":I am Calling, calling now~%;Spirit rise and falling"))))
      (format nil ":label~%::I am Calling, calling now~%:;Spirit rise and falling"))
  (let ((hash (make-hash-table)))
    (setf (gethash :|label|) #((format nil ":I am Calling, calling now~%;Spirit rise and falling")))
    (is (indite hash)
        (format nil ":label~%::I am Calling, calling now~%:;Spirit rise and falling"))))

(subtest "multiple data"
  (is (indite `(:|label| #(,(format nil "Save your tears. For the day.")
                           ,(format nil "When our pain is far behind.")))
              (format nil ":label ~a~%:label ~a"
                      "Save your tears. For the day."
                      "our pain is far behind.")))
  (is (indite `(:|label| #(,(format nil "On your feet.~%Come with me.")
                           ,(format nil "We are soldiers stand or die.")))
              (format nil ":label~%~a~%:label ~a"
                      "On your feet.~%Come with me."
                      "We are soldiers stand or die.")))
  (is (indite `(:|label| #(,(format nil "Save your fears.")
                           ,(format nil "Take your place.~%Save them for the judgement day.")))
              (format nil ":label~%~a~%:label ~a"
                      "Save your fears."
                      "Take your place.~%Save them for the judgement day.")))
  (is (indite `(:|label| #(,(format nil "Fast and free~%Follow me")
                           ,(format nil "Time to make the sacrifice~%We rise or fall ")))
              (format nil ":label~%~a~%:label~%~a"
                      "Fast and free~%Follow me"
                      "Time to make the sacrifice~%We rise or fall "))))
