(in-package :cl-user)
(defpackage mito.attachment.storage.s3
  (:use #:cl
        #:mito.attachment.storage)
  (:import-from #:zs3
                #:*s3-endpoint*
                #:*credentials*
                #:put-file
                #:put-stream
                #:delete-object)
  (:import-from #:alexandria
                #:once-only)
  (:export #:s3-storage
           #:s3-storage-access-key
           #:s3-storage-secret-key))
(in-package :mito.attachment.storage.s3)

(defclass s3-storage (storage)
  ((access-key :initarg :access-key
               :accessor s3-storage-access-key)
   (secret-key :initarg :secret-key
               :accessor s3-storage-secret-key))
  (:default-initargs
   :endpoint zs3::*s3-endpoint*))

(defmethod storage-file-url ((storage s3-storage) file-key)
  (format nil
          "https://~A/~A/~A"
          (storage-endpoint storage)
          (storage-bucket storage)
          file-key))

(defmacro with-s3-storage (storage &body body)
  (once-only (storage)
    `(let ((zs3::*s3-endpoint* (storage-endpoint ,storage))
           (zs3:*credentials* (list (s3-storage-access-key ,storage)
                                    (s3-storage-secret-key ,storage))))
       ,@body)))

(defmethod store-object-in-storage ((storage s3-storage) (object pathname) file-key)
  (with-s3-storage storage
    (zs3:put-file object (storage-bucket storage) file-key)))

(defmethod store-object-in-storage ((storage s3-storage) (object stream) file-key)
  (with-s3-storage storage
    (zs3:put-stream object (storage-bucket storage) file-key)))

(defmethod delete-object-from-storage ((storage s3-storage) file-key)
  (with-s3-storage storage
    (zs3:delete-object (storage-bucket storage) file-key)))
