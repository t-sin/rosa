(in-package :cl-user)
(defpackage rosa
  (:use :cl
        :proc-parse))
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
