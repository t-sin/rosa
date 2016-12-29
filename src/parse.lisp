(in-package :cl-user)
(defpackage rosa.parse
  (:use :cl
        :trivial-gray-streams)
  (:export :skim))
(in-package :rosa.parse)


(defstruct segment start end)

(defun read-label (stream)
  (values 'block-p 'label 'body))

(defun read-escape (stream)
  (read-char stream nil :eof))

(defun read-comment (stream)
  'skipped)

(defun read-plain-line (stream)
  'body)

(defun skim (stream)
  "read key-value data roughly **for internal**."
  (let ((in-block-label)
        (tmp-block)
        (data))
    (loop
       :for c := (read-char stream nil :eof)
       :until (eq c :eof)
       :do (case c
             (#\: (let ((peek (peek-char nil stream nil :eof)))
                    (cond ((or (char= peek #\;)
                               (char= peek #\:))
                           (read-escape stream)
                           ((eq peek :eof) (return))
                           (t (multiple-value-bind (block-p label body)
                                  (read-label stream)
                                (if block-p
                                    (setf in-block-label label))))))))
             (#\; (read-comment stream))
             (t (read-plain-line stream))))
    data))
