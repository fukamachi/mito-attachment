(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind (#+sbcl (warning #'muffle-warning))
    (defpackage mito-attachment
      (:nicknames #:mito.attachment)
      (:use #:cl))))
(in-package :mito-attachment)

(cl-reexport:reexport-from :mito.attachment.mixin)
(cl-reexport:reexport-from :mito.attachment.storage)
(cl-reexport:reexport-from :mito.attachment.storage.disk)
(cl-reexport:reexport-from :mito.attachment.storage.s3)
