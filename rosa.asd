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
  :version "1.0"
  :author "Shinichi TANAKA"
  :license "MIT"
  :depends-on ("alexandria"
               "trivial-gray-streams"
               "rosa/main")
  :description "Text labeling language"
  :in-order-to ((test-op (test-op :rosa/tests))))

(defsystem :rosa.cli
  :class :package-inferred-system
  :depends-on ("cl-yaml"
               "inquisitor"
               "jonathan"
               "rosa/cli")
  :entry-point "rosa/cli:entry-point")

(defsystem :rosa/tests
  :class :package-inferred-system
  :depends-on ("rove"
               "flexi-streams"
               "rosa/tests/basis"
               "rosa/tests/semantics"
               "rosa/tests/peruse"
               "rosa/tests/indite")
  :perform (test-op (o c) (uiop:symbol-call :rove ':run c)))
