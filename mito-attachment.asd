#|
  This file is a part of mito-attachment project.
  Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage mito-attachment-asd
  (:use :cl :asdf))
(in-package :mito-attachment-asd)

(defsystem mito-attachment
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:mito
               :aws-sdk
               :aws-sdk/services/s3
               :lack-component
               :trivial-mimes
               :quri
               :uuid
               :uiop
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "mito-attachment" :depends-on ("mixin" "storage" "storage-components"))
                 (:file "mixin" :depends-on ("storage" "util"))
                 (:file "storage")
                 (:module "storage-components"
                  :pathname "storage"
                  :depends-on ("storage" "util")
                  :components
                  ((:file "disk")
                   (:file "s3")))
                 (:file "util"))))
  :description "Mito mixin class for file management"
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
          seq))))
