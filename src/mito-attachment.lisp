(uiop:define-package #:mito-attachment
  (:nicknames #:mito.attachment)
  (:use #:cl)
  (:use-reexport #:mito.attachment.mixin
                 #:mito.attachment.storage
                 #:mito.attachment.storage.disk
                 #:mito.attachment.storage.s3))
(in-package #:mito-attachment)
