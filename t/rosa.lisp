(in-package :cl-user)
(defpackage rosa-test
  (:use :cl
        :rosa
        :prove))
(in-package :rosa-test)

;; NOTE: To run this test file, execute `(asdf:test-system :rosa)' in your Lisp.

(plan 4)

(defvar *test-string* "

for default context

:title Rosa - Text parts with metadata
:author Shinichi TANAKA
:date 2016-05-01

:date date2

ignored


:abstract

Rosa is a simple markup language for text parts with metadata.

:basis


Rosa's *named text* is a pair of strings, consists of **name** and **text**.

**Name** is a metadata.
And **text** is a data named with **name**.
**Text** is just one line string, or is multi line strings.

...

;comment

phew, english... I'm tired now...

")

(is (with-input-from-string (in *test-string*)
      (peruse-from-stream in))
    '(:|basis| ("Rosa's *named text* is a pair of strings, consists of **name** and **text**.

**Name** is a metadata.
And **text** is a data named with **name**.
**Text** is just one line string, or is multi line strings.

...")
      :|abstract| ("Rosa is a simple markup language for text parts with metadata.")
      :|date| ("2016-05-01" "date2")
      :|author| ("Shinichi TANAKA")
      :|title| ("Rosa - Text parts with metadata")
      :|+nil+| ("for default context")))

(subtest "first space is a separater"
  (is  (with-input-from-string (in *test-string*)
         (find-from-stream in :|title|))
       '("Rosa - Text parts with metadata")))

(subtest "empty lines at head or tail are removed"
  (is (with-input-from-string (in *test-string*)
        (find-from-stream in :|abstract|))
      '("Rosa is a simple markup language for text parts with metadata.")))

(subtest "holds duplicates"
  (is (with-input-from-string (in *test-string*)
        (find-from-stream in :|date|))
      '("2016-05-01" "date2"))
  (is (elt (with-input-from-string (in *test-string*)
             (find-from-stream in :|date|))
           0)
      "2016-05-01"))

(finalize)
