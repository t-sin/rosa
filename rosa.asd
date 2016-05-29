#|
  This file is a part of rosa project.
  Copyright (c) 2016 Shinichi TANAKA (shinichi.tanaka45@gmail.com)
|#

#|
  Simple markup language for named text parts

  Author: Shinichi TANAKA (shinichi.tanaka45@gmail.com)
|#

(in-package :cl-user)
(defpackage rosa-asd
  (:use :cl :asdf))
(in-package :rosa-asd)

(defsystem rosa
  :version "0.1"
  :author "Shinichi TANAKA"
  :license "MIT"
  :depends-on (:anaphora
               :proc-parse)
  :components ((:module "src"
                :components
                ((:file "rosa"))))
  :description "Simple markup language for named text parts"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op rosa-test))))
