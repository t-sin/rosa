(in-package :cl-user)
(uiop:define-package :rosa/tests/peruse
  (:use :cl
        :rosa
        :rosa/tests/util
        :prove))
(in-package :rosa/tests/peruse)

(subtest "normalize label"
  (diag "normalizer: identity")
  (with-input-from-string (in ":label identity")
    (is (peruse-as-plist in) '(:|label| #("identity"))
        :test #'plist-equal))

  (diag "normalizer: string-upcase")
  (with-input-from-string (in ":label upcased")
    (is (peruse-as-plist in #'string-upcase)
        '(:label #("upcased"))
        :test #'plist-equal))

  (diag "normalizer: replace l to r")
  (flet ((l-to-r (s)
           (with-output-to-string (out)
             (loop
                :for c :across s
                :do (if (char= c #\l)
                        (write-char #\r out)
                        (write-char c out))))))
    (with-input-from-string (in ":label l to r")
    (is (peruse-as-plist in #'l-to-r)
        '(:|raber| #("l to r"))
        :test #'plist-equal))))
