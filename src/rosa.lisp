(in-package :cl-user)
(defpackage rosa
  (:use :cl
        :proc-parse))
(in-package :rosa)


(defun parse-name (line)
  (with-string-parsing (line)
    (when (match? ":")
      (bind (name (skip-until (lambda (c) (char= c #\space))))
        (if (eofp)
            (return-from parse-name name)
            (progn
              (advance)
              (bind (str (skip-while (lambda (c) (char= c #\:))))
                (if (eofp)
                    (return-from parse-name (cons name str))
                    (return-from parse-name t)))))))))
