#|
  This file is a part of rosa project.
  Copyright (c) 2016 Shinichi TANAKA (shinichi.tanaka45@gmail.com)
|#

#|
  Text labeling language

  Author: Shinichi TANAKA (shinichi.tanaka45@gmail.com)
|#

(defsystem :rosa
  :class :package-inferred-system
  :version "0.1"
  :author "Shinichi TANAKA"
  :license "MIT"
  :depends-on ("rosa/main")
  :description "Text labeling language"
  :in-order-to ((test-op (test-op :rosa/tests))))

(register-system-packages :alexandria '(:alexandria))
(register-system-packages :anaphora '(:anaphora))
(register-system-packages :trivial-gray-streams '(:trivial-gray-streams))


(defsystem :rosa/tests
  :class :package-inferred-system
  :depends-on ("rove"
               "flexi-streams"
               "rosa/tests/basis"
               "rosa/tests/semantics"
               "rosa/tests/indite")
  :perform (test-op (o c) (uiop:symbol-call :rove ':run c)))
