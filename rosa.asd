#|
  This file is a part of rosa project.
  Copyright (c) 2016 Shinichi TANAKA (shinichi.tanaka45@gmail.com)
|#

#|
  Text labeling language

  Author: Shinichi TANAKA (shinichi.tanaka45@gmail.com)
|#

(in-package :cl-user)
(defpackage rosa-asd
  (:use :cl :asdf))
(in-package :rosa-asd)

(defsystem rosa
  :class :package-inferred-system
  :version "0.1"
  :author "Shinichi TANAKA"
  :license "MIT"
  :depends-on ("rosa/main"
               :alexandria
               :anaphora
               :trivial-gray-streams
               "")
  :description "Text labeling language"
  :in-order-to ((test-op (test-op rosa-test))))

(register-system-package "alexandria" '(:alexandria))
(register-system-package "anaphora" '(:anaphora))
(register-system-package "trivial-gray-streams"
                         '(:trivial-gray-streams))
