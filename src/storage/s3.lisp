(in-package :cl-user)
(defpackage mito.attachment.storage.s3
  (:use #:cl
        #:mito.attachment.storage)
  (:import-from #:mito-attachment.util
                #:slurp-stream)
  (:import-from #:aws-sdk
                #:*session*
                #:make-session
                #:make-credentials
                #:session-region
                #:session-credentials
                #:default-aws-credentials
                #:credentials-keys)
  (:import-from #:aws-sdk/services/s3
                #:put-object
                #:get-object
                #:delete-object)
  (:import-from #:aws-sign4
                #:*aws-credentials*
                #:aws-sign4)
  (:import-from #:flexi-streams
                #:make-flexi-stream
                #:make-in-memory-input-stream)
  (:import-from #:alexandria
                #:once-only
                #:remove-from-plist)
  (:export #:s3-storage))
(in-package :mito.attachment.storage.s3)

(defclass s3-storage (storage)
  ((session :initarg :session
            :accessor s3-storage-session)))

(defmethod initialize-instance :around ((storage s3-storage) &rest initargs
                                                             &key access-key secret-key session-token region session
                                                                  endpoint bucket
                                                             &allow-other-keys)
  (check-type bucket string)
  (check-type endpoint (or string null))
  (let ((session (or session
                     (aws:make-session :region region
                                       :credentials
                                       (when (or access-key secret-key session-token)
                                         (aws:make-credentials
                                           :access-key-id access-key
                                           :secret-access-key secret-key
                                           :session-token session-token))))))
    (apply #'call-next-method storage
           :session session
           :endpoint (or endpoint
                         (format nil "s3.~(~A~).amazonaws.com" (aws:session-region session)))
           (remove-from-plist initargs
                              '(:access-key :secret-key :session-token :region :session :endpoint)))))

(defmethod storage-file-url ((storage s3-storage) file-key)
  (format nil
          "https://~A.~A/~@[~A~]~A"
          (storage-bucket storage)
          (storage-endpoint storage)
          (storage-prefix storage)
          file-key))

(defmethod storage-file-signed-url ((storage s3-storage) file-key &key (method :get) (expires-in 900))
  (let* ((file-url (quri:uri (storage-file-url storage file-key)))
         (session (s3-storage-session storage))
         (credentials (or (aws:session-credentials session)
                          (aws:default-aws-credentials))))
    (multiple-value-bind (access-key secret-key session-token)
        (aws:credentials-keys credentials)
      (let ((aws-sign4:*aws-credentials*
              (lambda () (values access-key secret-key))))
        (aws-sign4:aws-sign4
          :region (aws:session-region session)
          :service "s3"
          :method method
          :host (quri:uri-host file-url)
          :path (quri:uri-path file-url)
          :params (when session-token
                    `(("X-Amz-Security-Token" . ,session-token)))
          :expires expires-in)))))

(defun s3-file-key (storage file-key)
  (format nil "~@[~A~]~A"
          (storage-prefix storage)
          file-key))

(defmacro with-s3-storage (storage &body body)
  (once-only (storage)
    `(let ((aws:*session* (s3-storage-session ,storage)))
       ,@body)))

(defmethod get-object-in-storage ((storage s3-storage) file-key)
  (let ((content
          (with-s3-storage storage
            (aws/s3:get-object :bucket (storage-bucket storage)
                               :key (s3-file-key storage file-key)))))
    (flex:make-flexi-stream content)))

(defmethod store-object-in-storage ((storage s3-storage) (object pathname) file-key)
  (with-open-file (in object :element-type '(unsigned-byte 8))
    (store-object-in-storage storage (slurp-stream in) file-key)))

(defmethod store-object-in-storage ((storage s3-storage) (object stream) file-key)
  (store-object-in-storage storage (slurp-stream object) file-key))

(defmethod store-object-in-storage ((storage s3-storage) (object sequence) file-key)
  (check-type object (array (unsigned-byte 8)))
  (with-s3-storage storage
    (aws/s3:put-object :bucket (storage-bucket storage)
                       :key (s3-file-key storage file-key)
                       :body object)))

(defmethod delete-object-from-storage ((storage s3-storage) file-key)
  (with-s3-storage storage
    (aws/s3:delete-object :bucket (storage-bucket storage)
                          :key (s3-file-key storage file-key))))
