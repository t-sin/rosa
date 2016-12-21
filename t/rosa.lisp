(in-package :cl-user)
(defpackage rosa-test
  (:use :cl
        :rosa
        :prove))
(in-package :rosa-test)

;; NOTE: To run this test file, execute `(asdf:test-system :rosa)' in your Lisp.

(plan 4)

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

(is (with-input-from-string (in *test-string*)
      (peruse in))
    '(:|title| ("Rosa - Named text parts")
      :|author| ("Shinichi TANAKA")
      :|date| ("2016-05-01" "2016-12-21")
      :|abstract| ("Rosa is a text labeling language.")
      :|body| ("Rosa is a language give key-value structure to text.
In other words, rosa is a language that give one name to text block.

Text written in rosa represent a ordered set of key-value pair.

Here, one pair in the set, it consist of **label** and **body**.
**Label** is a name of **body**.
We can consider **Label** as *key* and **body** as *value*.

:key value
;phew, engrish... I'm tired now...")))

(is (with-input-from-string (in *test-string*)
      (index in))
    '(:|title| :|author| :|date| :|abstract| :|body|))

(is (with-input-from-string (in *test-string*)
      (pick in :|title|))
    '("Rosa - Named text parts"))

(is (with-input-from-string (in *test-string*)
      (pick in :|date|))
    '("2016-05-01" "2016-12-21"))

