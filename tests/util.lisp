(in-package :cl-user)
(uiop:define-package :rosa/tests/util
  (:use :cl)
  (:export :plist-equal))
(in-package :rosa/tests/util)


(defun plist-equal (plist1 plist2)
  (and (= (length plist1) (length plist2))
       (loop
          :for (k v) :on plist1 :by #'cddr
          :always (equalp (getf plist1 k)
                          (getf plist2 k)))))
