(in-package :cl-user)
(uiop:define-package :rosa/main
  (:nicknames :rosa)
  (:use :cl
        :rosa/core)
  (:export :index
           :indite
           :peruse
           :peruse-as-plist
           :pick))
(in-package :rosa/main)


(defun peruse-as-plist (stream &optional (label-normalize-fn #'identity))
  "read key-value data as plist."
  (let ((data (peruse stream label-normalize-fn)))
    (loop
       :for k :being :each :hash-keys :of data :using (:hash-value v)
       :nconc (list k v))))

(defun index (stream)
  "returns all keys in `stream`."
  (let ((data (peruse stream)))
    (loop
       :for k :being :each :hash-keys :of data
       :collect k)))

(defun pick (stream label)
  "returns value corresponded `label`."
  (let ((data (peruse stream)))
    (gethash label data)))
