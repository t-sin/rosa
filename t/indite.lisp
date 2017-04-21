(in-package :cl-user)
(defpackage :rosa-test.indite
  (:use :cl
        :rosa
        :prove)
  (:import-from :alexandria
                :plist-hash-table))
(in-package :rosa-test.indite)


(defun test-indite (actual-plist expected-string)
  (is (indite actual-plist) expected-string)
  (let ((actual-hash (plist-hash-table actual-plist)))
    (is (indite actual-hash) expected-string)))

(subtest "nil maps to empty string"
  (test-indite '() ""))

(subtest "mapping of inline label (not has newline)"
  (test-indite '(:|label| #("ghost in the shell"))
               (format nil ":label ghost in the shell~%")))

(subtest "mapping of block label (has newline)"
  (test-indite `(:|label| #(,(format nil "I hear a voice,~%hear a voice calling out to me")))
               (format nil ":label~%I hear a voice,~%hear a voice calling out to me~%")))

(subtest "escape ':' and ';' in block"
  (test-indite `(:|label| #(,(format nil ":I am Calling, calling now~%;Spirit rise and falling")))
               (format nil ":label~%::I am Calling, calling now~%:;Spirit rise and falling~%")))

(subtest "multiple data"
  (diag "inline - inline")
  (test-indite `(:|label| #(,(format nil "Save your tears. For the day.")
                            ,(format nil "When our pain is far behind.")))
               (format nil ":label ~a~%:label ~a~%"
                       "Save your tears. For the day."
                       "When our pain is far behind."))
  (diag "block - inline")
  (test-indite `(:|label| #(,(format nil "On your feet.~%Come with me.")
                            ,(format nil "We are soldiers stand or die.")))
               (format nil ":label~%~a~%~a~%:label ~a~%"
                       "On your feet." "Come with me."
                       "We are soldiers stand or die."))
  (diag "inline - block")
  (test-indite `(:|label| #(,(format nil "Save your fears.")
                            ,(format nil "Take your place.~%Save them for the judgement day.")))
               (format nil ":label ~a~%:label~%~a~%~a~%"
                       "Save your fears."
                       "Take your place." "Save them for the judgement day."))
  (diag "block - block")
  (test-indite `(:|label| #(,(format nil "Fast and free~%Follow me")
                            ,(format nil "Time to make the sacrifice~%We rise or fall ")))
               (format nil ":label~%~a~%~a~%:label~%~a~%~a~%"
                       "Fast and free" "Follow me"
                       "Time to make the sacrifice" "We rise or fall ")))
