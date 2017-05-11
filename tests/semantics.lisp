(in-package :cl-user)
(uiop:define-package :rosa/tests/semantics
  (:use :cl
        :rosa
        :rosa/tests/util
        :prove))
(in-package :rosa/tests/semantics)

(plan 9)


(defun perusing-test (actual expected)
  (with-input-from-string (in actual)
    (is (peruse-as-plist in) expected :test #'plist-equal)))

(subtest "empty text returns empty data"
  (perusing-test "" nil)
  (perusing-test " " nil)
  (perusing-test "abcd" nil)
  (perusing-test (format nil "~a" #\tab) nil)
  (perusing-test (format nil "~a" #\newline) nil)
  (perusing-test (format nil "~a" #\return) nil)
  (perusing-test (format nil "~%~%~%~%") nil))

(subtest "inline labels"
  (diag "inline label is consists of two parts; label string and body string")

  (subtest "inline label is represent as regexp `^:([a-zA-Z0-9][a-zA-Z0-9-_]*) (.+)$`"
    (perusing-test ":abcd is read as label"
                   '(:|abcd| #("is read as label")))
    (perusing-test ":abCD is read as label"
                   '(:|abCD| #("is read as label")))
    (perusing-test ":ABCD is read as label"
                   '(:ABCD #("is read as label")))
    (perusing-test ":abcd-efg is read as label"
                   '(:|abcd-efg| #("is read as label")))
    (perusing-test ":abcd- is read as label"
                   '(:|abcd-| #("is read as label")))
    (perusing-test ":abcd_ is read as label"
                   '(:|abcd_| #("is read as label"))))

  (subtest "these are not inline labels"
    (perusing-test ":-abcd is not read as label" nil)
    (perusing-test ":_abcd is not read as label" nil))

  (subtest "body can include any characters except line-break"
    (perusing-test ":label body" '(:|label| #("body")))
    (perusing-test ":label golden-body" '(:|label| #("golden-body")))
    (perusing-test ":label heart of gold" '(:|label| #("heart of gold")))
    (perusing-test (format nil ":label so long~%and thanks for all the fish")
                   '(:|label| #("so long")))

    (subtest "special cases with labels"
      (perusing-test (format nil ":label so long~%:label2 and thanks for all the fish")
                     '(:|label| #("so long")
                       :|label2| #("and thanks for all the fish")))
      (perusing-test (format nil ":label so long~%:label2~%and thanks for all the fish")
                     '(:|label| #("so long")
                       :|label2| #("and thanks for all the fish"))))))

(subtest "list notation for multiple inline label"
  (perusing-test (format nil ":label>~%- spaaaaaaaace")
                 '(:|label| #("spaaaaaaaace")))
  (perusing-test (format nil ":label>~%- spaaaaaaaace~%- in space")
                 '(:|label| #("spaaaaaaaace" "in space")))
  (perusing-test (format nil ":label>~%- spaaaaaaaace~%- in space~%")
                 '(:|label| #("spaaaaaaaace" "in space")))

  (subtest "empty list is nil"
    (perusing-test (format nil ":label>~%") nil)
    (perusing-test (format nil ":label>") nil)

    (subtest "lines starts with only '- ' are regarded as `list`"
      (perusing-test (format nil ":label>~%-spaaaaaaaace") nil)
      (perusing-test (format nil ":label>~% - spaaaaaaaace") nil)))

  (subtest "empty lines are ignored"
    (perusing-test (format nil ":label>~%- spaaaaaaaace~%")
                   '(:|label| #("spaaaaaaaace")))
    (perusing-test (format nil ":label>~%- spaaaaaaaace~%~%")
                   '(:|label| #("spaaaaaaaace")))
    (perusing-test (format nil ":label>~%- spaaaaaaaace~%~%- in space")
                   '(:|label| #("spaaaaaaaace" "in space")))))

(subtest "block labels"
  (diag "block label is consists of two parts; label line and following body line(s)")

  (subtest "block label is represent as regexp `^:([a-zA-Z0-9][a-zA-Z0-9-_]*)$`"
    (diag "single appearance of label line")
    (perusing-test (format nil ":abcd") '(:|abcd| #("")))
    (perusing-test (format nil ":abcd-efg") '(:|abcd-efg| #("")))
    (perusing-test (format nil ":abcd-") '(:|abcd-| #(""))))

  (subtest "block label is represent as regexp `^:([a-zA-Z0-9][a-zA-Z0-9-_]*)$`"
    (perusing-test (format nil ":abcd~%is read as label")
                   '(:|abcd| #("is read as label")))
    (perusing-test (format nil ":abCD~%is read as label")
                   '(:|abCD| #("is read as label")))
    (perusing-test (format nil ":ABCD~%is read as label")
                   '(:ABCD #("is read as label")))
    (perusing-test (format nil ":abcd-efg~%is read as label")
                   '(:|abcd-efg| #("is read as label")))
    (perusing-test (format nil ":abcd-~%is read as label")
                   '(:|abcd-| #("is read as label")))
    (perusing-test (format nil ":abcd_~%is read as label")
                   '(:|abcd_| #("is read as label"))))

  (subtest "these are not label"
    (perusing-test (format nil ":-abcd~%is not read as label") nil)
    (perusing-test (format nil ":_abcd~%is not read as label") nil))

  (subtest "body can include any lines except both kind of labels"
    (perusing-test (format nil ":label~%body") '(:|label| #("body")))
    (perusing-test (format nil ":label~%golden~%body")
                   `(:|label| #(,(format nil "golden~%body"))))
    (perusing-test (format nil ":label~%heart of gold")
                   '(:|label| #("heart of gold")))
    (perusing-test (format nil ":label~%so long~%and thanks for all the fish")
                   `(:|label| #(,(format nil "so long~%and thanks for all the fish")))))

  (subtest "when newline is placed at front of EOF, body ends at previous char of *the newline*"
    (perusing-test (format nil ":label~%so long~%")
                   '(:|label| #("so long"))))

  (subtest "when newline is not placed at front of EOF, body ends at previous char of *EOF*"
    (perusing-test (format nil ":label~%so long")
                   '(:|label| #("so long"))))

  (subtest "when newline is placed at front of label, body ends at previous char of *the newline*"
    (perusing-test (format nil ":label~%so long~%:label2 and thanks for all the fish")
                   '(:|label| #("so long")
                     :|label2| #("and thanks for all the fish"))))

  (subtest "when block has no body lines, body is empty string"
    (perusing-test (format nil ":label~%~%")
                   '(:|label| #("")))
    (perusing-test (format nil ":label~%~%:label2 text")
                   '(:|label| #("") :|label2| #("text"))))

  (subtest "line number"
    (subtest "two"
      (perusing-test (format nil ":label~%one~%two")
                     `(:|label| #(,(format nil "one~%two"))))
      (perusing-test (format nil ":label~%one~%two~%")
                     `(:|label| #(,(format nil "one~%two"))))
      (perusing-test (format nil ":label~%one~%two~%:label2 text")
                     `(:|label| #(,(format nil "one~%two")) :|label2| #("text")))

      (perusing-test (format nil ":label~%~%~%")
                     `(:|label| #(,(format nil "~%"))))
      (perusing-test (format nil ":label~%~%~%:label2 text")
                     `(:|label| #(,(format nil "~%")) :|label2| #("text"))))

    (subtest "three"
      (perusing-test (format nil ":label~%one~%two~%three")
                     `(:|label| #(,(format nil "one~%two~%three"))))
      (perusing-test (format nil ":label~%one~%two~%three~%")
                     `(:|label| #(,(format nil "one~%two~%three"))))
      (perusing-test (format nil ":label~%one~%two~%three~%:label2 text")
                     `(:|label| #(,(format nil "one~%two~%three")) :|label2| #("text")))

      (perusing-test (format nil ":label~%~%~%~%")
                     `(:|label| #(,(format nil "~%~%"))))
      (perusing-test (format nil ":label~%~%~%~%:label2 text")
                     `(:|label| #(,(format nil "~%~%")) :|label2| #("text"))))))

(subtest "labels must be at line head. there are not labels"
  (perusing-test " :label body" nil)
  (perusing-test "examples: Arthur, Ford and Trillian" nil)
  (perusing-test "examples:are bellow" nil)

  (perusing-test (format nil " :label~%body") nil)
  (perusing-test (format nil "examples:~%Arthur, Ford and Trillian") nil)
  (perusing-test (format nil "examples:are~%bellow") nil)

  (perusing-test (format nil ":block~%body~% :ignore ignored")
                 `(:|block| #(,(format nil "body~% :ignore ignored"))))
  (perusing-test (format nil ":block~%body~% :ignore~%ignored")
                 `(:|block| #(,(format nil "body~% :ignore~%ignored")))))

(subtest "comment"
  (subtest "comment starts with colon in block body, are ignored"
    (perusing-test (format nil ":block~%;NGAHHHHHH")
                   `(:|block| #(,(format nil ""))))

    (perusing-test (format nil ":block~%oh,~%;NGAHHHHHH~%deep thought.")
                   `(:|block| #(,(format nil "oh,~%deep thought."))))
    (perusing-test (format nil ":block~%oh,~%;NGAHHHHHH~%;NGAHHHHHH~%deep thought.")
                   `(:|block| #(,(format nil "oh,~%deep thought."))))
    (perusing-test (format nil ":block~%oh,~%;NGAHHHHHH~%deep~%;NGAHHHHHH~%thought.")
                   `(:|block| #(,(format nil "oh,~%deep~%thought.")))))

  (subtest "comment starts at line-head"
      (perusing-test (format nil ":block~%oh,~%;NGAHHHHHH~%deep thought.")
                     `(:|block| #(,(format nil "oh,~%deep thought."))))
      (perusing-test (format nil ":block~%oh,~% ;NGAHHHHHH~%deep thought.")
                     `(:|block| #(,(format nil "oh,~% ;NGAHHHHHH~%deep thought.")))))

  (subtest "comment line consists of a couple; string and newline"
    (perusing-test (format nil ":block~%oh,~%~%;NGAHHHHHH~%~%deep thought.")
                   `(:|block| #(,(format nil "oh,~%~%~%deep thought."))))

    (subtest "comment line is regarded as empty string"
      (perusing-test (format nil ":block~%;NGAHHHHHH~%;NGAHHHHHH~%:label body")
                     `(:|block| #(,(format nil ""))
                       :|label| #("body")))

      (perusing-test (format nil ":block~%;NGAHHHHHH~%;NGAHHHHHH")
                     `(:|block| #(,(format nil ""))))
      (perusing-test (format nil ":block~%;NGAHHHHHH~%;NGAHHHHHH~%")
                     `(:|block| #(,(format nil ""))))))

  (subtest "when block ends with comment, block body does not include eol"
    (perusing-test (format nil ":block~%oh,~%;NGAHHHHHH~%deep thought.~%;NGAHHHHHH")
                   `(:|block| #(,(format nil "oh,~%deep thought."))))
    (perusing-test (format nil ":block~%oh,~%;NGAHHHHHH~%deep thought.~%;NGAHHHHHH~%:label body")
                   `(:|block| #(,(format nil "oh,~%deep thought."))
                     :|label| #("body")))))

(subtest "escape sequences"
  (subtest "colon escaping"
    (subtest "escaping is a plain line"
      (perusing-test ":: is colon" nil))

    (subtest "colon escaping in block label"
      (perusing-test (format nil ":block~%:: is colon")
                     `(:|block| #(,(format nil ": is colon"))))))

  (subtest "semicolon escaping"
    (subtest "escaping is a plain line"
      (perusing-test ":: is colon" nil))

    (subtest "semicolon escaping in block label"
      (perusing-test (format nil ":block~%:; is semicolon")
                     `(:|block| #(,(format nil "; is semicolon"))))))

  (subtest "escaping is elable only at head of line"
    (perusing-test (format nil ":block~%::: is colon colon")
                   `(:|block| #(,(format nil ":: is colon colon"))))
    (perusing-test (format nil ":block~% :: is colon colon")
                   `(:|block| #(,(format nil " :: is colon colon"))))

    (perusing-test (format nil ":block~%:;; is semicolon semicolon")
                   `(:|block| #(,(format nil ";; is semicolon semicolon"))))
    (perusing-test (format nil ":block~% :; is colon semicolon")
                   `(:|block| #(,(format nil " :; is colon semicolon")))))

  (subtest "escape sequences in a row (issue #12)"
    (perusing-test (format nil ":block~%:; is semicolon~%:: is colon~%")
                   `(:|block| #(,(format nil "; is semicolon~%: is colon"))))
    (perusing-test (format nil ":block~%:; is semicolon~%:: is colon~%::: is colon colon")
                   `(:|block| #(,(format nil "; is semicolon~%: is colon~%:: is colon colon"))))))

(subtest "plain line not in block are ignored"
  (perusing-test (format nil ":label text~%foo") '(:|label| #("text")))
  (perusing-test (format nil ":label~%text~%foo")
                 `(:|label| #(,(format nil "text~%foo"))))

  (perusing-test (format nil "foo~%:label text") '(:|label| #("text")))
  (perusing-test (format nil "foo~%:label~%text") '(:|label| #("text")))

  (perusing-test (format nil "foo~%:label text~%bar") '(:|label| #("text")))
  (perusing-test (format nil "foo~%:label~%text~%bar")
                 `(:|label| #(,(format nil "text~%bar")))))

(subtest "when block ends with newline, eol removes newline at tail"
  (perusing-test (format nil ":block~%line1")
                 `(:|block| #(,(format nil "line1"))))
  (perusing-test (format nil ":block~%line1~%")
                 `(:|block| #(,(format nil "line1"))))

  (perusing-test (format nil ":block~%line1~%line2")
                 `(:|block| #(,(format nil "line1~%line2"))))
  (perusing-test (format nil ":block~%line1~%line2~%")
                 `(:|block| #(,(format nil "line1~%line2"))))

  (perusing-test (format nil ":block~%line1~%")
                 `(:|block| #(,(format nil "line1"))))
  (perusing-test (format nil ":block~%line1~%~%")
                 `(:|block| #(,(format nil "line1~%")))))


(finalize)

