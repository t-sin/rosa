(in-package :cl-user)
(defpackage rosa-test-util
  (:use :cl)
  (:export :plist-equal))
(in-package :rosa-test-util)


(defun plist-equal (plist1 plist2)
  (and (= (length plist1) (length plist2))
       (loop
          :for (k v) :on plist1 :by #'cddr
          :always (equalp (getf plist1 k)
                          (getf plist2 k)))))
