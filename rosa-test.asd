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
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "rosa"))))
  :description "Test system for rosa"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
