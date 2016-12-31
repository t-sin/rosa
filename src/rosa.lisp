(in-package :cl-user)
(defpackage rosa
  (:use :cl
        :trivial-gray-streams)
  (:import-from :rosa.parse
                :peruse)
  (:export :index
           :peruse
           :peruse-as-plist
           :pick))
(in-package :rosa)


(defun peruse-as-plist (stream)
  "read key-value data as plist."
  )


(defun index (stream)
  "returns all keys in `stream`."
  )

(defun pick (stream label)
  "returns value corresponded `label`."
  )
