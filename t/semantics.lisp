(in-package :cl-user)
(defpackage rosa-semantics-test
  (:use :cl
        :rosa
        :prove))
(in-package :rosa-semantics-test)

(plan 6)


(subtest "empty text returns empty data"
  (is (peruse-as-plist "") nil)
  (is (peruse-as-plist " ") nil)
  (is (peruse-as-plist "abcd") nil)
  (is (peruse-as-plist (format nil "~a" #\tab)) nil)
  (is (peruse-as-plist (format nil "~a" #\newline)) nil)
  (is (peruse-as-plist (format nil "~a" #\return)) nil)
  (is (peruse-as-plist (format nil "~%~%~%~%")) nil))

(subtest "inline labels"
  (diag "inline label is consists of two parts; label string and body string")

  (subtest "inline label is represent as regexp `^:([a-z][a-z-]*) (.+)$`"
    (is (peruse-as-plist ":abcd is read as label")
        '(:|abcd| "is read as label"))
    (is (peruse-as-plist ":abcd-efg is read as label")
        '(:|abcd-efg| "is read as label"))
    (is (peruse-as-plist ":abcd- is read as label")
        '(:|abcd-| "is read as label")))

  (subtest "these are not inline labels"
    (is (peruse-as-plist ":-abcd is not read as label") nil)
    (is (peruse-as-plist ":ABCD is not read as label") nil)
    (is (peruse-as-plist ":abCD is not read as label") nil)
    (is (peruse-as-plist ":abcd_ is not read as label") nil)
    (is (peruse-as-plist ":abCD is not read as label") nil))

  (subtest "body can include any characters except line-break"
    (is (peruse-as-plist ":label body") '(:|label| "body"))
    (is (peruse-as-plist ":label golden-body") '(:|label| "golden-body"))
    (is (peruse-as-plist ":label heart of gold") '(:|label| "heart of gold"))
    (is (peruse-as-plist (format nil ":label so long~%and thanks for all the fish")) '(:|label| "so long"))

    (subtest "special cases with labels"
      (is (peruse-as-plist (format nil ":label so long~%:label2 and thanks for all the fish"))
          '(:|label| "so long"
            :|label2| "and thanks for all the fish"))
      (is (peruse-as-plist (format nil ":label so long~%:label2~%and thanks for all the fish"))
          '(:|label| "so long"
            :|label2| "and thanks for all the fish")))))

(subtest "block labels"
  (diag "block label is consists of two parts; label line and following body line(s)")

  (subtest "block label is represent as regexp `^:([a-z][a-z-]*)$`"
    (diag "single appearance of label line is actually ignored")
    (is (peruse-as-plist (format nil ":abcd")) nil)
    (is (peruse-as-plist (format nil ":abcd-efg")) nil)
    (is (peruse-as-plist (format nil ":abcd-")) nil))

  (subtest "block label is represent as regexp `^:([a-z][a-z-]*)$`"
    (is (peruse-as-plist (format nil ":abcd~%is read as label"))
        '(:|abcd| "is read as label"))
    (is (peruse-as-plist (format nil ":abcd-efg~%is read as label"))
        '(:|abcd-efg| "is read as label"))
    (is (peruse-as-plist (format nil ":abcd-~%is read as label"))
        '(:|abcd-| "is read as label")))

  (subtest "these are not label"
    (is (peruse-as-plist (format nil ":-abcd~%is not read as label")) nil)
    (is (peruse-as-plist (format nil ":ABCD~%is not read as label")) nil)
    (is (peruse-as-plist (format nil ":abCD~%is not read as label")) nil)
    (is (peruse-as-plist (format nil ":abcd_~%is not read as label")) nil)
    (is (peruse-as-plist (format nil ":abCD~%is not read as label")) nil))

  (subtest "body can include any lines except both kind of labels"
    (is (peruse-as-plist (format nil ":label~%body")) '(:|label| "body"))
    (is (peruse-as-plist (format nil ":label~%golden~%body"))
        `(:|label| ,(format nil "golden~%body")))
    (is (peruse-as-plist (format nil ":label~%heart of gold"))
        '(:|label| "heart of gold"))
    (is (peruse-as-plist (format nil ":label~%so long~%and thanks for all the fish"))
        `(:|label| ,(format nil "so long~%and thanks for all the fish"))))

  (subtest "special cases with labels"
      (is (peruse-as-plist (format nil ":label~%so long~%:label2 and thanks for all the fish"))
          '(:|label| "so long"
            :|label2| "and thanks for all the fish"))
      (is (peruse-as-plist (format nil ":label~%so long~%:label2~%and thanks for all the fish"))
          '(:|label| "so long"
            :|label2| "and thanks for all the fish"))))

(subtest "comment")
(subtest "escape sequences"
  (subtest "colon escaping")
  (subtest "semicolon escaping"))


(finalize)

