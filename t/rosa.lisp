(in-package :cl-user)
(defpackage rosa-test
  (:use :cl
        :rosa
        :rosa-test-util
        :prove)
  (:import-from :alexandria
                :set-equal)
  (:import-from :flexi-streams
                :make-flexi-stream
                :string-to-octets
                :with-input-from-sequence))
(in-package :rosa-test)

;; NOTE: To run this test file, execute `(asdf:test-system :rosa)' in your Lisp.

(plan 5)


(defvar *test-string* "

this liens are ignored.

:title Rosa - text labeling language
:author Shinichi TANAKA
:date 2016-05-01
:date 2016-12-21

:abstract

Rosa is a text labeling language.

:body

Rosa is a language give key-value structure to text.
In other words, rosa is a language that give one name to text block.

Text written in rosa represent a ordered set of key-value pair.

Here, one pair in the set, it consist of **label** and **body**.
**Label** is a name of **body**.
We can consider **Label** as *key* and **body** as *value*.

;comment

::key value
:;phew, engrish... I'm tired now...
")

;;; peruse API; all data read at once.
(is (with-input-from-string (in *test-string*)
      (peruse in))
    (let ((hash (make-hash-table)))
      (setf (gethash :|title| hash) #("Rosa - text labeling language"))
      (setf (gethash :|author| hash) #("Shinichi TANAKA"))
      (setf (gethash :|date| hash) #("2016-05-01" "2016-12-21"))
      (setf (gethash :|abstract| hash) #("
Rosa is a text labeling language.
"))
      (setf (gethash :|body| hash) #("
Rosa is a language give key-value structure to text.
In other words, rosa is a language that give one name to text block.

Text written in rosa represent a ordered set of key-value pair.

Here, one pair in the set, it consist of **label** and **body**.
**Label** is a name of **body**.
We can consider **Label** as *key* and **body** as *value*.


:key value
;phew, engrish... I'm tired now..."))
      hash)
    :test #'equalp)

;;; peruse API; return eazy-to-use structure
(is (with-input-from-string (in *test-string*)
      (peruse-as-plist in))
    '(:|title| #("Rosa - text labeling language")
      :|author| #("Shinichi TANAKA")
      :|date| #("2016-05-01" "2016-12-21")
      :|abstract| #("
Rosa is a text labeling language.
")
      :|body| #("
Rosa is a language give key-value structure to text.
In other words, rosa is a language that give one name to text block.

Text written in rosa represent a ordered set of key-value pair.

Here, one pair in the set, it consist of **label** and **body**.
**Label** is a name of **body**.
We can consider **Label** as *key* and **body** as *value*.


:key value
;phew, engrish... I'm tired now..."))
    :test #'plist-equal)

;;; indexing API; listing labels.
(is (with-input-from-string (in *test-string*)
      (index in))
    '(:|title| :|author| :|date| :|abstract| :|body|)
    :test #'set-equal)

;;; picking up API; pickinck up body(ies) with specified label.
(is (with-input-from-string (in *test-string*)
      (pick in :|date|))
    #("2016-05-01" "2016-12-21")
    :test #'equalp)

;;; rosa supports gray streams
(is (with-input-from-sequence (in (string-to-octets *test-string*))
      (pick (make-flexi-stream in) :|title|))
    #("Rosa - text labeling language")
    :test #'equalp)

;;; rosa write data into a string
(with-input-from-string (in *test-string*)
  (let ((data (peruse in)))
    (is (peruse (indite data)) data)))


(finalize)
