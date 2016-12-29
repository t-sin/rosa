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

(defun read-label (stream)
  (values 'block-p 'label 'body))
(defun read-escape (stream)
  'body)
(defun read-comment (stream)
  'skipped)
(defun read-plain-line (stream)
  'body)

(defun skim (stream)
  "read key-value data roughly **for internal**."
  (let ((in-block-p nil)
        (tmp-block)
        (data))
    (loop
       :for c := (read-char stream nil :eof)
       :until (eq c :eof)
       :do (case c
             (#\: (let ((peek (peek-char nil stream nil :eof)))
                    (if (or (char= peek #\;)
                            (char= peek #\:))
                        (read-escape stream)
                        (read-label stream))))
             (#\; (read-comment stream))
             (t (read-plain-line stream))))))


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
