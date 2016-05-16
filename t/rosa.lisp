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


(subtest "empty text"
  (let ((s ""))
    (with-input-from-string (in s)
      (is (peruse-from-stream in) nil))
    (with-input-from-string (in s)
      (is (find-from-stream in :name) nil)))

  (let ((s "
"))
    (with-input-from-string (in s)
      (is (peruse-from-stream in) nil))
    (with-input-from-string (in s)
      (is (find-from-stream in :name) nil))))


(subtest "inline text"
  (subtest "first space is a separater"
    (let ((s ":name text"))
      (with-input-from-string (in s)
        (is (peruse-from-stream in) '(:|name| ("text"))))
      (with-input-from-string (in s)
        (is (find-from-stream in :|name|) '("text")))))

  (subtest "second or successor space is included text"
    (let ((s ":name text1 text2"))
      (with-input-from-string (in s)
        (is (peruse-from-stream in) '(:|name| ("text1 text2"))))
      (with-input-from-string (in s)
        (is (find-from-stream in :|name|) '("text1 text2"))))))


(subtest "block text"
  (subtest "range of block text"
    (let ((s ":name
text"))
      (with-input-from-string (in s)
        (is (peruse-from-stream in) '(:|name| ("text"))))
      (with-input-from-string (in s)
        (is (find-from-stream in :|name|) '("text"))))

    (let ((s ":name
line1
line2"))
      (with-input-from-string (in s)
        (is (peruse-from-stream in) '(:|name| ("line1
line2"))))
      (with-input-from-string (in s)
        (is (find-from-stream in :|name|) '("line1
line2"))))

    (let ((s ":name
line1
line2
:name2"))
      (with-input-from-string (in s)
        (is (peruse-from-stream in) '(:|name| ("line1
line2"))))
      (with-input-from-string (in s)
        (is (find-from-stream in :|name|) '("line1
line2")))))

  (subtest "empty lines at head or tail are removed"
    (let ((s ":name

text
"))
      (with-input-from-string (in s)
        (is (peruse-from-stream in) '(:|name| ("text"))))
      (with-input-from-string (in s)
        (is (find-from-stream in :|name|) '("text"))))

    (let ((s ":name
text

"))
      (with-input-from-string (in s)
        (is (peruse-from-stream in) '(:|name| ("text"))))
      (with-input-from-string (in s)
        (is (find-from-stream in :|name|) '("text"))))

        (let ((s ":name

text

"))
      (with-input-from-string (in s)
        (is (peruse-from-stream in) '(:|name| ("text"))))
      (with-input-from-string (in s)
        (is (find-from-stream in :|name|) '("text")))))

  (subtest "comments"))

(subtest "escape sequences")

(subtest "holds duplicates"
  (let ((s ":name rose
:name rosa"))
    (with-input-from-string (in s)
      (is (peruse-from-stream in) '(:|name| ("rose" "rosa"))))
    (with-input-from-string (in s)
      (is (find-from-stream in :|name|) '("rose" "rosa"))))
  (let ((s ":title
The name of the rose
:title
Il nome della rosa"))
    (with-input-from-string (in s)
      (is (peruse-from-stream in)
          '(:|title| ("The name of the rose"
                      "Il nome della rosa"))))
    (with-input-from-string (in s)
      (is (find-from-stream in :|title|)
          '("The name of the rose" "Il nome della rosa")))))

(finalize)
