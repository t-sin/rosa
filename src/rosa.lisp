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


(defstruct segment start end)

(defun skim (stream))

(defun peruse-as-plist (stream))

(defun peruse (stream))

(defun index (stream))

(defun pick (stream label))
