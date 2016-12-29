(in-package :cl-user)
(defpackage rosa
  (:use :cl
        :proc-parse
        :trivial-gray-streams)
  (:export :index
           :peruse
           :peruse-as-plist
           :pick))
(in-package :rosa)


(defun peruse-as-plist (stream)
  "read key-value data as plist."
  )

(defun peruse (stream)
  "read key-value data."
  )

(defun index (stream)
  "returns all keys in `stream`."
  )

(defun pick (stream label)
  "returns value corresponded `label`."
  )
