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

:title Rosa - Named text parts
:author Shinichi TANAKA
:date 2016-05-01

:date date2

ignored


:abstract

Rosa is a simple markup language for named text parts.

:basis


Rosa's *named text* is a pair of strings, consists of **name** and **text**.

**Name** is a name of **text**.
**Text** is just one line string, or is multi line strings.

...

;comment

phew, english... I'm tired now...

")

(is (with-input-from-string (in *test-string*)
      (peruse-from-stream in))
    '(:|basis| ("Rosa's *named text* is a pair of strings, consists of **name** and **text**.

**Name** is a name of **text**.
**Text** is just one line string, or is multi line strings.

...")
      :|abstract| ("Rosa is a simple markup language for named text parts.")
      :|date| ("2016-05-01" "date2")
      :|author| ("Shinichi TANAKA")
      :|title| ("Rosa - Named text parts")
      :|+nil+| ("for default context")))

(subtest "empty lines"
  (with-input-from-string (in "")
    (is (peruse-from-stream in) nil)
    (is (find-from-stream in :name) nil))
  (with-input-from-string (in "
")
    (is (peruse-from-stream in) nil)
    (is (find-from-stream in :name) nil)))

(subtest "first space is a separater"
  (is  (with-input-from-string (in *test-string*)
         (find-from-stream in :|title|))
       '("Rosa - Named text parts")))

(subtest "empty lines at head or tail are removed"
  (is (with-input-from-string (in *test-string*)
        (find-from-stream in :|abstract|))
      '("Rosa is a simple markup language for named text parts.")))

(subtest "holds duplicates"
  (is (with-input-from-string (in *test-string*)
        (find-from-stream in :|date|))
      '("2016-05-01" "date2"))
  (is (elt (with-input-from-string (in *test-string*)
             (find-from-stream in :|date|))
           0)
      "2016-05-01"))

(finalize)
