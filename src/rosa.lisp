(in-package :cl-user)
(defpackage rosa
  (:use :cl
        :proc-parse)
  (:import-from :anaphora
                :aif
                :atypecase
                :it))
(in-package :rosa)


(defparameter *default-name* :|+nil+|)
(defparameter *comment-name* :|+comment+|)

(defun parse-name (line)
  (with-string-parsing (line)
    (flet ((return% (value) (return-from parse-name value))
           (2char= (c1 &optional c2)
             (if c2
                 (and (char= (current) c1)
                      (char= (peek) c2))
                 (char= (current) c1)))
           (space-p (c) (char= c #\space))
           (true-with-char (c) c))
      (cond ((2char= #\: #\:) (return% nil))
            ((2char= #\; #\;) (return% nil))
            ((2char= #\;) (return% :comment))
            ((2char= #\:)
             (advance*)
             (bind (name (skip-until #'space-p))
               (if (eofp)
                   (return% name)
                   (progn
                     (advance)
                     (bind (str (skip-while #'true-with-char))
                       (return% (cons name str)))))))
            (t (return% nil))))))

(defun run-through-stream (stream block-fn inline-fn)
  (let ((block-p t)
         (block-name *default-name*)
         (block-text))
    (labels ((to-keyword (s) (intern s :keyword))
             (end-of-block ()
               (when block-text
                 (funcall block-fn block-name block-text))
               (setf block-p nil
                     block-name *default-name*
                     block-text nil)))
      (loop
         :for line := (read-line stream nil :eof)
         :until (eq line :eof)
         :finally (end-of-block)
         :do (atypecase (parse-name line)
               (keyword (end-of-block))
               (null (when block-p
                       (push line block-text)))
               (string (end-of-block)
                       (setf block-p t
                             block-name (to-keyword it)))
               (cons (when block-p
                       (end-of-block))
                     (funcall inline-fn (to-keyword (car it)) (cdr it))
                     (setf block-name *default-name*)))))))

(defun trim-empty-string (strlist)
  (let ((start (position "" strlist :test-not #'string=))
         (end (position "" strlist :test-not #'string= :from-end t)))
    (cond ((and (null start) (null end)) nil)
          ((and (zerop start) (= (length strlist) end)) strlist)
          (t (subseq strlist start (1+ end))))))

(defun stringify (strlist)
  (format nil "~{~a~^~%~}"
          (nreverse (trim-empty-string strlist))))

(defun add-to-named (name string named)
  (flet ((set-to-named (value)
           (setf (getf named name) value)
           named))
    (if named
        (aif (getf named name)
             (set-to-named (append it (list string)))
             (set-to-named (list string)))
        (set-to-named (list string)))))

(defun peruse-from-stream (stream)
  (let ((named))
    (flet ((add-block-to-named (block-name block-text)
             (when (trim-empty-string block-text)
               (setf named (add-to-named block-name (stringify block-text) named))))
           (add-inline-to-named (inline-name inline-text)
             (setf named (add-to-named inline-name inline-text named))))
      (run-through-stream stream #'add-block-to-named #'add-inline-to-named)
      named)))

