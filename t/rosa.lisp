(in-package :cl-user)
(defpackage rosa-test
  (:use :cl
        :rosa
        :prove))
(in-package :rosa-test)

;; NOTE: To run this test file, execute `(asdf:test-system :rosa)' in your Lisp.

(plan 1)

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


(finalize)
