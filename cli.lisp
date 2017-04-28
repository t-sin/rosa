(in-package :cl-user)
(uiop:define-package :rosa/cli
  (:use :cl)
  (:import-from :rosa/main
                :index
                :peruse
                :pick)
  (:import-from :cl-yaml
                :emmit-to-string)
  (:import-from :inquisitor
                :detect-end-of-line
                :make-external-format)
  (:import-from :jonathan
                :to-json)
  (:export :entry-point
           :cli-main))
(in-package :rosa/cli)

(defun print-usage ()
  (format *error-output* "Rosa - text labeling language CLI

usage: rosa index [OPTIONS] [FILE]
       rosa pick [OPTIONS] LABEL [FILE]
       rosa dump [OPTIONS] [FILE]

DESCRIPTION
    Rosa extract key-value structure from input. Key as 'label' and value as 'body'.

    If not supplied FILE, rosa reads from standard input.

COMMANDS
    Each commands have its shorthand; the first letter. For instance, shorthand of
    `index` is `i`.

    rosa index [FILE]
        List all labels from input. By default, formatted as line-based plain text.

    rosa pick LABEL [FILE]
        Pick up the value(s) corresponding to the label, from input. By default,
        rosa returns only first body of all appearance.

    rosa dump [FILE]
        Parse and print entire key-value data from input.

FORMATTING OPTIONS
    Output formatting type. By default, output formatted as S-expression.

      -s
          Format output as S-expression.
      -j
          Format output as JSON.
      -y
          Format output as YAML.
PICKING UP OPTIONS
    Available with pick command.

      -a
          If supplied, all bodies are printed as formatting type.

      -n NUM
          Specify number of bodies picking up from head. By default, no supplied,
          NUM set as 1.

      -t
          Tell rosa to pick up tail.

"))

(defstruct option
  command format-type pick-label pick-all pick-nth pick-tail pathname)

(defparameter +commands+
  '((:index "i" "index")
    (:pick "p" "pick")
    (:dump "d" "dump")))

(defun commandp (s)
  (find s +commands+
        :test #'(lambda (i s) (member i s :test #'string=))))

(defun error-and-exit (s)
  (format t "~a~%~%" s)
  (uiop:quit 1))

(defun parse-argv (argv)
  (let ((option (make-option)))
    (flet ((optionp (a)
             (char= (aref a 0) #\-))
           (find-format-type (a)
             (setf (option-format-type option)
                   (cond ((string= a "-s") :sexp)
                         ((string= a "-j") :json)
                         ((string= a "-y") :yaml)))))
      (let* ((s (first argv))
             (command (first (commandp s))))
        (if (or (not s) (not command))
            (error-and-exit (format nil "unknown command: ~s" s))
            (setf (option-command option) command)))
      (loop
         :for n :from 1 :below (length argv)
         :with command := (option-command option)
         :for a string := (nth n argv)
         :do (if (optionp a)
                 (cond ((member a '("-s" "-j" "-y") :test #'string=)
                        (when (not (option-format-type option))
                          (find-format-type a)))
                       ((and (eq command :pick)
                             (string= a "-a"))
                        (setf (option-pick-all option) t))
                       ((and (eq command :pick)
                             (string= a "-t"))
                        (setf (option-pick-tail option) t))
                       ((and (eq command :pick)
                             (string= a "-n"))
                        (incf n)
                        (handler-case
                            (setf (option-pick-nth option) (parse-integer (nth n argv)))
                          (condition (c)
                            (declare (ignore c))
                            (error-and-exit (format nil "~s is not positive integer"
                                                    (nth n argv))))))
                       (t (error-and-exit (format nil "unknown option: ~s" a))))
                 (cond ((and (eq command :pick)
                             (not (option-pick-label option)))
                        (setf (option-pick-label option) (intern a :keyword)))
                       (t (if (not (option-pathname option))
                              (setf (option-pathname option) (pathname a))
                              (error-and-exit "too many arguments"))))))
      option)))

(defmacro with-input ((var pathname) &body body)
  `(if ,pathname
       (if (probe-file ,pathname)
           (with-open-file (,var ,pathname
                            :direction :input
                            :external-format (make-external-format
                                              :utf8
                                              (detect-end-of-line (pathname ,pathname))))
             ,@body)
           (error-and-exit (format nil "no such file: ~s" ,pathname)))
       (let ((,var *standard-input*))
         ,@body)))

(defun index-labels (option)
  (with-input (in (option-pathname option))
    (let ((labels (rosa:index in)))
      (case (option-format-type option)
        (:sexp (format t "~s~%" labels))
        (:json (format t "~a~%"
                       (to-json (loop
                                   :for l :in labels
                                   :collect (symbol-name l)))))
        (:yaml (format t "~a~%"
                       (emit-to-string
                        (loop
                           :for l :in labels
                           :collect (symbol-name l)))))
        (t (loop :for l :in labels :do (format t "~a~%" l)))))))

(defun pick-bodies (option)
  (with-input (in (option-pathname option))
    (flet ((print-body (body format-type)
             (case format-type
               (:sexp (format t "~s~%" body))
               (:json (format t "~a~%" (to-json body)))
               (:yaml (format t "~a~%" (emit-to-string body)))
               (t (format t "~a~%" body))))
           (expected-body (bodies)
             (let ((pick-nth (option-pick-nth option)))
               (handler-case
                   (cond ((option-pick-all option) bodies)
                         ((option-pick-tail option) (aref bodies (1- (length bodies))))
                         (pick-nth (aref bodies pick-nth))
                         (t (aref bodies 0)))
                 (condition (c)
                   (error-and-exit (format nil "~a" c)))))))
      (let* ((expected (expected-body (rosa:pick in (option-pick-label option)))))
        (if (stringp expected)
            (print-body expected nil)
            (print-body expected
                        (or (option-format-type option) :sexp)))))))

(defun dump-all (option)
  (with-input (in (option-pathname option))
    (let ((format-type (or (option-format-type option) :sexp)))
      (flet ((string-keyed-data ()
               (let ((newhash (make-hash-table :test #'equal)))
                 (maphash (lambda (k v)
                            (setf (gethash (symbol-name k) newhash) v))
                          (rosa:peruse in))
                 newhash)))
        (case format-type
          (:sexp (format t "~s~%" (rosa:peruse-as-plist in)))
          (:json (format t "~a~%" (to-json (rosa:peruse-as-plist in))))
          (:yaml (format t "~a~%" (emit-to-string (string-keyed-data)))))))))

(defun cli-main (&rest argv)
  (if (< (length argv) 1)
      (progn
        (print-usage)
        (uiop:quit))
      (let ((option (parse-argv argv)))
        (case (option-command option)
          (:index (index-labels option))
          (:pick (pick-bodies option))
          (:dump (dump-all option))))))

(defun entry-point ()
  "Entry point for asdf:program-op."
  (cli-main (uiop:command-line-arguments)))
