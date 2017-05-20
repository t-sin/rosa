(in-package :cl-user)
(uiop:define-package :rosa/core
  (:use :cl)
  (:import-from :alexandria
                :plist-hash-table)
  (:import-from :trivial-gray-streams
                :fundamental-character-input-stream)
  (:export :indite
           :peruse))
(in-package :rosa/core)


(defmacro with-linereader ((instream) &body body)
  `(let ((linereader (if (subtypep (type-of ,instream)
                               'fundamental-character-input-stream)
                     #'(lambda () (multiple-value-bind (line eof)
                                      (stream-read-char ,instream)
                                    (and line eof)))
                     #'(lambda () (read-line ,instream nil :eof)))))
     (declare (ignorable linereader))
     ,@body))

(defun escaped-line-p (line)
  (and (> (length line) 2)
       (or (char= (char line 1) #\:)
           (char= (char line 1) #\;))))

(defun remove-eol (string)
  "Remove EOL positioned in front of EOF"
  (let* ((tail-pos (1- (length string))))
    (cond ((< tail-pos 0) "")
          ((char= (char string tail-pos) #\newline)
           (subseq string 0 tail-pos))
          (t string))))

(defun push-body (hash label body label-normalize-fn)
  (let ((key (intern (funcall label-normalize-fn label) :keyword)))
    (if (gethash key hash)
        (vector-push-extend body (gethash key hash))
        (let ((val (make-array 1 :initial-element body :fill-pointer 1 :adjustable t)))
          (setf (gethash key hash) val)))))

(defun peruse (stream &optional (label-normalize-fn #'identity))
  "parse stream and return parsed rosa data as hash table"
  (let ((rosa-data (make-hash-table))
        (block-type)
        (block-label)
        (block-text))
    (labels ((push-body-when-block ()
               (when (eq block-type :block)
                 (push-body rosa-data block-label
                            (remove-eol (get-output-stream-string block-text))
                            label-normalize-fn)))
             (update-state-as-inline (label text)
               (push-body-when-block)
               (setf block-type nil
                     block-label nil)
               (push-body rosa-data label text label-normalize-fn))
             (update-state-as-list (label)
               (push-body-when-block)
               (setf block-type :list
                     block-label label))
             (update-state-as-block (label)
               (push-body-when-block)
               (setf block-type :block
                     block-label label
                     block-text (make-string-output-stream)))
             (add-to-list (line)
               (when (and (>= (length line) 2)
                          (string= (subseq line 0 2) "- "))
                 (push-body rosa-data block-label
                            (subseq line 2) label-normalize-fn)))
             (append-line (line)
               (case block-type
                 (:block (format block-text "~a~%" line))
                 (:list (add-to-list line))))
             (colon-line (s)
               (if (escaped-line-p s)
                   (append-line (subseq s 1))
                   (let ((space-position (position #\space s)))
                     (cond (space-position
                            (let ((label (subseq s 1 space-position))
                                  (text (subseq s (1+ space-position))))
                              (update-state-as-inline label text)))
                           ((char= (char s (1- (length s))) #\>)
                            (update-state-as-list (subseq s 1 (1- (length s)))))
                           (t (update-state-as-block (subseq s 1)))))))
             (otherwise-line (s) (append-line s)))
      (with-linereader (stream)
        (loop :named parse
           :for line := (read-line stream nil :eof)
           :do (cond ((eq line :eof) (progn
                                       (update-state-as-block block-label)
                                       (return-from parse rosa-data)))
                     ((and (> (length line) 0)
                           (char= (char line 0) #\:))
                      (colon-line line))
                     ((and (> (length line) 0)
                           (char= (char line 0) #\;)) :do-nothing)
                     (t (otherwise-line line))))))))

(defmethod indite ((data hash-table))
  "write key-value data into string."
  (with-output-to-string (out)
    (labels ((block-p (s) (find #\newline s))
             (print-inline (name body)
               (format out ":~a ~a~%" name body))
             (print-block (name body)
               (format out ":~a~%~a~%" name
                       (with-output-to-string (out)
                         (with-input-from-string (in body)
                           (loop
                              :for ch := (read-char in nil :eof)
                              :until (eq ch :eof)
                              :do (cond ((char= ch #\:) (format out "::"))
                                        ((char= ch #\;) (format out ":;"))
                                        (t (write-char ch out))))))))
             (print-label (label-name body)
               (if (block-p body)
                   (print-block label-name body)
                   (print-inline label-name body))))
      (loop
         :for k :being :each :hash-keys :of data :using (:hash-value v)
         :do (if (stringp v)
                 (print-label k v)
                 (loop :for s :across v :do (print-label k s)))))))

(defmethod indite ((data list))
  "indite plist."
  (indite (plist-hash-table data)))
