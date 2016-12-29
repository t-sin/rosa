(in-package :cl-user)
(defpackage rosa.parse
  (:use :cl
        :trivial-gray-streams)
  (:export :skim))
(in-package :rosa.parse)


(defstruct segment start end)

(defun read-label (stream)
  (loop
     :for c := (peek-char nil stream nil :eof)
     )
  (values 'block-p 'label 'body))

(defun read-block (stream)
  'body)

(defun read-escape (stream)
  (read-char stream nil :eof))

(defun read-comment (stream)
  'skipped)

(defun skim (stream)
  "read key-value data roughly **for internal**."
  (loop
     :for c := (read-char stream nil :eof)
     :with data := (make-hash-table)
     :until (eq c :eof)
     :finally (return data)
     :when (char= c #\:)
     :do (let ((peek (peek-char nil stream nil :eof)))
           (cond
             ((or (char= peek #\;) (char= peek #\:)) (read-escape stream))
             ((eq peek :eof) (return))
             (t (multiple-value-bind (block-p label body)
                    (read-label stream)
                  (if block-p
                      (setf (gethash label data) (read-block stream))
                      (setf (gethash label data) body))))))))
