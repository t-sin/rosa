(in-package :cl-user)
(defpackage rosa.parse
  (:use :cl
        :trivial-gray-streams)
  (:export :skim))
(in-package :rosa.parse)


(defmacro run-until-chars (chlist chvar instream outstream reader-type &body do-form)
  (let ((run-form `(let ((reader (if (subtypep (type-of ,instream)
                                               'fundamental-character-input-stream)
                                     #'(lambda () (stream-read-char ,instream))
                                     #'(lambda () (read-char ,instream nil :eof))))
                         (peeker (if (subtypep (type-of ,instream)
                                               'fundamental-character-input-stream)
                                     #'(lambda () (stream-peek-char ,instream))
                                     #'(lambda () (peek-char nil ,instream nil :eof)))))
                     (declare (ignorable reader peeker))
                     (loop :named run-until-chars
                        :for ,chvar := (funcall ,(ecase reader-type
                                                   (:read 'reader)
                                                   (:peek 'peeker)))
                        :while (and (not (eq ,chvar :eof))
                                    ,@(loop :for c :in chlist :collect `(char/= ,chvar ,c)))
                        :do (progn ,@do-form)))))
    (if outstream
        `(with-output-to-string (,outstream) ,run-form)
        run-form)))

(defmacro cond-escape-sequence (peek-fn eof escape-seq otherwise)
  (let ((peek (gensym)))
   `(let ((,peek (funcall ,peek-fn)))
      (cond ((eq ,peek :eof) ,eof)
            ((or (char= ,peek #\:) (char= ,peek #\;)) ,escape-seq)
            (t ,otherwise)))))

(defun read-label-identifier (stream)
  (labels ((identifier-first-char-p (ch)
             (or (and (<= (char-code #\a) (char-code ch))
                      (>= (char-code #\z) (char-code ch)))
                 (and (<= (char-code #\0) (char-code ch))
                      (>= (char-code #\9) (char-code ch)))))
           (identifier-char-p (ch)
             (or (identifier-first-char-p ch)
                 (char= ch #\-))))
    (with-output-to-string (out)
      (loop
         :for c := (peek-char nil stream nil :eof)
         :with first-p := t
         :while (and (not (eq c :eof))
                     (if first-p
                         (progn
                           (setf first-p nil)
                           (identifier-char-p c))
                         (identifier-first-char-p c)))
         :do (write-char (read-char stream nil :eof) out)))))

(defun read-label (stream)
  "returns (block-p label body rest)"
  (cond-escape-sequence
   (lambda () (peek-char nil stream nil :eof))
   (values nil nil nil (run-until-chars (#\newline) c stream out :read
                         (write-char c out)))
   (values nil nil nil (run-until-chars (#\newline) c stream out :read
                         (write-char c out)))
   (let ((label (read-label-identifier stream))
         (ch (read-char stream nil :eof)))
     (cond ((eq ch :eof)                ; block but next is EOF ;(
            (return-from read-label (values t label nil nil)))
           ((char= ch #\space)          ; inline
            (values nil label
                    (run-until-chars (#\newline) c stream out :read
                      (write-char c out))
                    nil))
           ((char= ch #\newline)        ; truly, block
            (values t label nil nil))
           (t                     ; regard invalid identifier as plain
            (let* ((rest- (run-until-chars (#\newline) c stream out :read
                            (write-char c out)))
                   (rest (format nil "~a~c~a" label ch rest-)))
              (values nil nil nil rest)))))))

(defun read-block (stream)
  "returns body"
  (run-until-chars nil c stream out :peek
    (flet ((skip-before-newline ()
             (run-until-chars (#\newline) ch1 stream nil :peek
               (funcall reader)))
           (read-before-newline ()
             (run-until-chars (#\newline) ch2 stream nil :peek
               (write-char (funcall reader) out))))
      (cond ((eq c :eof) (return-from run-until-chars))
            ((char= c #\newline)
             (progn (funcall reader)
                    (let ((ch (funcall peeker)))
                      (cond ((eq ch :eof)
                             (progn (terpri out)
                                    (return-from run-until-chars)))
                            ((char= ch #\newline) (terpri out))
                            ((char= ch #\:)
                             (progn (funcall reader)
                                    (cond-escape-sequence peeker
                                                          (return-from run-until-chars)
                                                          (progn (terpri out)
                                                                 (read-before-newline))
                                                          (return-from run-until-chars))))
                            ((char= ch #\;) (skip-before-newline))
                            (t (format out "~c~c" #\newline (funcall reader)))))))
            (t (write-char (funcall reader) out))))))

(defun skim (stream)
  "read key-value data roughly **for internal**."
  (loop
     :for c := (read-char stream nil :eof)
     :with data := (make-hash-table)
     :until (eq c :eof)
     :finally (return-from skim data)
     :when (char= c #\:)
     :do (format t "~s~%" c)
     :do (multiple-value-bind (block-p label body rest)
             (read-label stream)
           (format t "~s ~s ~s ~s~%" block-p label body rest)
           (unless rest
               (if block-p
                   (setf (gethash (intern label :keyword) data) (read-block stream))
                   (setf (gethash (intern label :keyword) data) body))))))


#+nil((or (char= peek #\;) (char= peek #\:)) (read-escape stream))
