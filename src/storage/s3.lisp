(in-package :cl-user)
(defpackage mito.attachment.storage.s3
  (:use #:cl
        #:mito.attachment.storage)
  (:import-from #:zs3
                #:*s3-region*
                #:*s3-endpoint*
                #:*credentials*
                #:region-endpoint
                #:put-file
                #:put-stream
                #:delete-object)
  (:import-from #:alexandria
                #:once-only)
  (:export #:s3-storage
           #:s3-storage-credentials))
(in-package :mito.attachment.storage.s3)

(defclass s3-storage (storage)
  ((access-key :initarg :access-key
               :accessor s3-storage-access-key)
   (secret-key :initarg :secret-key
               :accessor s3-storage-secret-key)
   (session-token :initarg :session-token
                  :initform nil
                  :accessor s3-storage-session-token)
   (region :initarg :region
           :initform zs3:*s3-region*
           :accessor s3-storage-region))
  (:default-initargs
   :endpoint zs3:*s3-endpoint*))

(defgeneric s3-storage-credentials (storage)
  (:method ((storage s3-storage))
    (values
     (s3-storage-access-key storage)
     (s3-storage-secret-key storage)
     (s3-storage-session-token storage))))

(defmethod storage-file-url ((storage s3-storage) file-key)
  (format nil
          "https://~A.~A/~@[~A~]~A"
          (storage-bucket storage)
          (storage-endpoint storage)
          (storage-prefix storage)
          file-key))

(defun s3-file-key (storage file-key)
  (format nil "~@[~A~]~A"
          (storage-prefix storage)
          file-key))

(defmacro with-s3-storage (storage &body body)
  (once-only (storage)
    `(let ((zs3:*s3-region* (s3-storage-region ,storage))
           ;; XXX: ZS3 ignores the bucket name if zs3:*s3-endpoint* is different
           ;;   from the returned value of zs3::region-endpoint.
           (zs3:*s3-endpoint* (zs3::region-endpoint (s3-storage-region ,storage)))
           (zs3:*credentials* (multiple-value-list
                                (s3-storage-credentials ,storage))))
       ,@body)))

(defmethod get-object-in-storage ((storage s3-storage) file-key)
  (with-s3-storage storage
    (zs3:get-object (storage-bucket storage) (s3-file-key storage file-key)
                    :output :stream)))

(defmethod store-object-in-storage ((storage s3-storage) (object pathname) file-key)
  (with-s3-storage storage
    (zs3:put-file object (storage-bucket storage) (s3-file-key storage file-key))))

(defmethod store-object-in-storage ((storage s3-storage) (object stream) file-key)
  (with-s3-storage storage
    (zs3:put-stream object (storage-bucket storage) (s3-file-key storage file-key))))

(defmethod store-object-in-storage ((storage s3-storage) (object sequence) file-key)
  (check-type object (array (unsigned-byte 8)))
  (with-s3-storage storage
    (zs3:put-vector object (storage-bucket storage) (s3-file-key storage file-key))))

(defmethod delete-object-from-storage ((storage s3-storage) file-key)
  (with-s3-storage storage
    (zs3:delete-object (storage-bucket storage) (s3-file-key storage file-key))))
