(in-package :cl-user)
(defpackage rosa.parse
  (:use :cl
        :trivial-gray-streams)
  (:import-from :anaphora
                :aif)
  (:export :peruse))
(in-package :rosa.parse)


(defmacro with-reader (instream &body body)
  `(let ((reader (if (subtypep (type-of ,instream)
                               'fundamental-character-input-stream)
                     #'(lambda () (stream-read-char ,instream))
                     #'(lambda () (read-char ,instream nil :eof))))
         (peeker (if (subtypep (type-of ,instream)
                               'fundamental-character-input-stream)
                     #'(lambda () (stream-peek-char ,instream))
                     #'(lambda () (peek-char nil ,instream nil :eof))))
         (unreader (if (subtypep (type-of ,instream)
                                 'fundamental-character-input-stream)
                       #'(lambda (c) (stream-unread-char ,instream c))
                       #'(lambda (c) (unread-char c ,instream)))))
     (declare (ignorable reader peeker unreader))
     ,@body))

(defun escaped-line-p (line)
  (and (> (length line) 2)
       (or (char= (char line 1) #\:)
           (char= (char line 1) #\;))))

(defun push-body (hash label body)
  (let ((key (intern label :keyword)))
    (if (gethash key hash)
        (vector-push-extend body (gethash key hash))
        (let ((val (make-array 1 :initial-element body :fill-pointer 1 :adjustable t)))
          (setf (gethash key hash) val)))))

(defun peruse (stream)
  "parse and return parsed rosa data as hash table"
  (let ((rosa-data (make-hash-table))
        (block-label)
        (block-text))
    (labels ((update-state-as-inline (label text)
               (setf block-label nil
                     block-text (make-string-output-stream))
               (push-body rosa-data label text))
             (update-state-as-block (label)
               (when block-label
                 (push-body rosa-data block-label (get-output-stream-string block-text)))
               (setf block-label label))
             (append-line-to-block (line)
               (when block-label
                 (format block-text "~a~%" line)))
             (colon-line (s)
               (if (escaped-line-p s)
                   (append-line-to-block (subseq s 1))
                   (aif (position #\space s)
                        (update-state-as-inline (subseq s 1 anaphora:it)
                                                (subseq s (1+ anaphora:it)))
                        (update-state-as-block (subseq s 1)))))
             (otherwise-line (s) (append-line-to-block s)))
      (loop :named parse
         :for line := (read-line stream nil :eof)
         :do (cond ((eq line :eof) (progn
                                     (update-state-as-block block-label)
                                     (return-from parse rosa-data)))
                   ((and (> (length line) 0)
                         (char= (char line 0) #\:))
                    (colon-line line))
                   ((and (> (length line) 0)
                         (char= (char line 0) #\;)) 'do-nothing)
                   (t (otherwise-line line)))))))
