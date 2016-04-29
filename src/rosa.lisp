(in-package :cl-user)
(defpackage rosa
  (:use :cl
        :proc-parse)
  (:import-from :anaphora
                :aif
                :it))
(in-package :rosa)


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
            ((2char= #\;) (return% t))
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

(defun add-to-name-list (name string name-list)
  (flet ((set-to-name-list (value)
           (setf (getf name-list name) value)
           name-list))
    (if name-list
        (aif (getf name-list name)
             (set-to-name-list (append it (list string)))
             (set-to-name-list (list string)))
        (set-to-name-list (list string)))))
