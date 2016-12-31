(in-package :cl-user)
(defpackage rosa-semantics-test
  (:use :cl
        :rosa
        :prove))
(in-package :rosa-semantics-test)

(plan 6)


(defun perusing-test (actual expected)
  (with-input-from-string (in actual)
    (is (peruse-as-plist in) expected :test #'equalp)))

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

  (subtest "inline label is represent as regexp `^:([a-z0-9][a-z0-9-]*) (.+)$`"
    (perusing-test ":abcd is read as label"
        '(:|abcd| "is read as label"))
    (perusing-test ":abcd-efg is read as label"
        '(:|abcd-efg| "is read as label"))
    (perusing-test ":abcd- is read as label"
        '(:|abcd-| "is read as label")))

  (subtest "these are not inline labels"
    (perusing-test ":-abcd is not read as label" nil)
    (perusing-test ":ABCD is not read as label" nil)
    (perusing-test ":abCD is not read as label" nil)
    (perusing-test ":abcd_ is not read as label" nil)
    (perusing-test ":abCD is not read as label" nil))

  (subtest "body can include any characters except line-break"
    (perusing-test ":label body" '(:|label| "body"))
    (perusing-test ":label golden-body" '(:|label| "golden-body"))
    (perusing-test ":label heart of gold" '(:|label| "heart of gold"))
    (perusing-test (format nil ":label so long~%and thanks for all the fish") '(:|label| "so long"))

    (subtest "special cases with labels"
      (perusing-test (format nil ":label so long~%:label2 and thanks for all the fish")
          '(:|label| "so long"
            :|label2| "and thanks for all the fish"))
      (perusing-test (format nil ":label so long~%:label2~%and thanks for all the fish")
          '(:|label| "so long"
            :|label2| "and thanks for all the fish")))))

(subtest "block labels"
  (diag "block label is consists of two parts; label line and following body line(s)")

  (subtest "block label is represent as regexp `^:([a-z0-9][a-z0-9-]*)$`"
    (diag "single appearance of label line is actually ignored")
    (perusing-test (format nil ":abcd") nil)
    (perusing-test (format nil ":abcd-efg") nil)
    (perusing-test (format nil ":abcd-") nil))

  (subtest "block label is represent as regexp `^:([a-z0-9][a-z0-9-]*)$`"
    (perusing-test (format nil ":abcd~%is read as label")
        '(:|abcd| "is read as label"))
    (perusing-test (format nil ":abcd-efg~%is read as label")
        '(:|abcd-efg| "is read as label"))
    (perusing-test (format nil ":abcd-~%is read as label")
        '(:|abcd-| "is read as label")))

  (subtest "these are not label"
    (perusing-test (format nil ":-abcd~%is not read as label") nil)
    (perusing-test (format nil ":ABCD~%is not read as label") nil)
    (perusing-test (format nil ":abCD~%is not read as label") nil)
    (perusing-test (format nil ":abcd_~%is not read as label") nil)
    (perusing-test (format nil ":abCD~%is not read as label") nil))

  (subtest "body can include any lines except both kind of labels"
    (perusing-test (format nil ":label~%body") '(:|label| "body"))
    (perusing-test (format nil ":label~%golden~%body")
        `(:|label| ,(format nil "golden~%body")))
    (perusing-test (format nil ":label~%heart of gold")
        '(:|label| "heart of gold"))
    (perusing-test (format nil ":label~%so long~%and thanks for all the fish")
        `(:|label| ,(format nil "so long~%and thanks for all the fish"))))

  (subtest "special cases with labels"
      (perusing-test (format nil ":label~%so long~%:label2 and thanks for all the fish")
          '(:|label| "so long"
            :|label2| "and thanks for all the fish"))
      (perusing-test (format nil ":label~%so long~%:label2~%and thanks for all the fish")
          '(:|label| "so long"
            :|label2| "and thanks for all the fish"))))

(subtest "comment"
  (subtest "comment starts with colon, and are ignored"
    (perusing-test ";comment" nil)
    (perusing-test "; comment" nil))

  (subtest "comments in block body are ignored"
    (perusing-test (format nil ":block~%oh,~%;comment~%deep thought.")
        `(:|block| ,(format nil "~%oh,~%deep thought.")))
    (perusing-test (format nil ":block~%oh,~%;comment1~%;comment2~%deep thought.")
        `(:|block| ,(format nil "~%oh,~%deep thought.")))
    (perusing-test (format nil ":block~%oh,~%;comment1~%deep~%;comment2~%thought.")
        `(:|block| ,(format nil ":block~%oh,~%deep~%thought.")))
    (perusing-test (format nil ":block~%oh,~%;comment1~%deep thought.~%;comment2")
        `(:|block| ,(format nil ":block~%oh,~%deep thought.")))))

(subtest "escape sequences"
  (subtest "colon escaping"
    (subtest "escaping is a plain line"
      (perusing-test ":: is colon" nil))

    (subtest "colon escaping in block label"
      (perusing-test (format nil ":block~%:: is colon")
          `(:|block| ,(format nil ": is colon")))

      (subtest "escaping is elable only at head of line"
        (perusing-test (format nil ":block~%::: is colon colon")
            `(:|block| ,(format nil ":: is colon colon")))
        (perusing-test (format nil ":block~% :: is colon colon")
            `(:|block| ,(format nil " :: is colon colon"))))))

  (subtest "semicolon escaping"
    (subtest "escaping is a plain line"
      (perusing-test ":: is colon" nil))

    (subtest "semicolon escaping in block label"
      (perusing-test (format nil ":block~%:; is semicolon")
          `(:|block| ,(format nil "; is semicolon")))

      (subtest "escaping is elable only at head of line"
        (perusing-test (format nil ":block~%:;; is semicolon semicolon")
            `(:|block| ,(format nil ";; is semicolon semicolon")))
        (perusing-test (format nil ":block~% ;; is semicolon semicolon")
            `(:|block| ,(format nil " ;; is semicolon semicolon")))))))

(subtest "plain line not in block are ignored"
  (perusing-test (format nil ":label text~%foo") '(:|label| "text"))
  (perusing-test (format nil "foo~%:label text") '(:|label| "text"))
  (perusing-test (format nil "foo~%:label text~%bar") '(:|label| "text"))

  (perusing-test (format nil ":label~%text~%foo") '(:|label| "text"))
  (perusing-test (format nil "foo~%:label~%text") '(:|label| "text"))
  (perusing-test (format nil "foo~%:label~%text~%bar") '(:|label| "text")))


(finalize)

