#|
  This file is a part of rosa project.
  Copyright (c) 2016 Shinichi TANAKA (shinichi.tanaka45@gmail.com)
|#

(in-package :cl-user)
(defpackage rosa-test-asd
  (:use :cl :asdf))
(in-package :rosa-test-asd)

(defsystem rosa-test
  :author "Shinichi TANAKA"
  :license "MIT"
  :depends-on (:rosa
               :alexandria
               :flexi-streams
               :prove)
  :components ((:module "t"
                :components
                ((:file "util")
                 (:test-file "rosa" :depends-on ("util"))
                 (:test-file "semantics" :depends-on ("util"))
                 (:test-file "indite"))))
  :description "Test system for rosa"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
