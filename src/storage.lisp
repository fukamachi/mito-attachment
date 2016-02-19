(in-package :cl-user)
(defpackage mito.attachment.storage
  (:use #:cl)
  (:export #:*storage*
           #:storage
           #:storage-bucket
           #:storage-endpoint
           #:storage-file-url
           #:store-object-in-storage
           #:delete-object-from-storage))
(in-package :mito.attachment.storage)

(defvar *storage*)

(defclass storage ()
  ((bucket :initarg :bucket
           :accessor storage-bucket)
   (endpoint :initarg :endpoint
             :initform nil
             :accessor storage-endpoint)))

(defgeneric storage-file-url (storage file-key))

(defgeneric store-object-in-storage (storage object file-key))

(defgeneric delete-object-from-storage (storage file-key))
