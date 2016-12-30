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
  "returns (body label-p)"
  (run-until-chars nil c stream out :read
    (flet ((skip-to-eol ()
             (run-until-chars (#\newline) ch1 stream nil :peek
               (funcall reader)))
           (read-to-eol ()
             (run-until-chars (#\newline) ch2 stream nil :peek
               (write-char (funcall reader) out)))
           (return-when-label-found ()
             (return-from read-block
               (values (get-output-stream-string out) t))))
      (cond ((eq c :eof) (return-from run-until-chars))
            ((char= c #\newline) (multiple-value-bind (body label-p)
                                     (read-block stream)
                                   (unless (zerop (length body))
                                     (format out "~%~a" body))
                                   (when label-p
                                       (return-when-label-found))))
            ((char= c #\:) (cond-escape-sequence peeker
                                                 (return-from run-until-chars)
                                                 (read-to-eol)
                                                 (progn
                                                   (unread-char #\: stream)
                                                   (return-when-label-found))))
            ((char= c #\;) (skip-to-eol))
            (t (progn
                 (write-char c out)
                 (read-to-eol)))))))

(defun push-body (hash label body)
  (let ((key (intern label :keyword)))
    (if (gethash key hash)
        (vector-push-extend body (gethash key hash))
        (let ((val (make-array 1 :initial-element body :fill-pointer 1 :adjustable t)))
          (setf (gethash key hash) val)))))

(defun skim (stream)
  "read key-value data roughly **for internal**."
  (loop
     :for c := (read-char stream nil :eof)
     :with data := (make-hash-table)
     :until (eq c :eof)
     :finally (return-from skim data)
     :when (char= c #\:)
     :do (multiple-value-bind (block-p label body rest)
             (read-label stream)
           (unless rest
             (if block-p
                 (push-body data label (read-block stream))
                 (push-body data label body))))))
