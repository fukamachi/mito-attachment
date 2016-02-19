(in-package :cl-user)
(defpackage mito.attachment.mixin
  (:use #:cl)
  (:import-from #:mito.attachment.storage
                #:storage
                #:*storage*
                #:storage-file-url
                #:store-object-in-storage
                #:delete-object-from-storage)
  (:import-from #:trivial-mimes
                #:mime-lookup
                #:mime-file-type)
  (:import-from #:mito
                #:dao-table-mixin
                #:save-dao
                #:delete-dao)
  (:export #:attachment
           #:attachment-file-key
           #:attachment-content-type
           #:attachment-file-size
           #:attachment-file-url
           #:attachment-content))
(in-package :mito.attachment.mixin)

(defclass attachment ()
  ((file-key :col-type (:varchar 255)
             :initarg :file-key
             :accessor attachment-file-key)
   (content-type :col-type (:varchar 255)
                 :initarg :content-type
                 :accessor attachment-content-type)
   (file-size :col-type :integer
              :initarg :file-size
              :accessor attachment-file-size)
   (content :ghost t
            :initarg :content
            :accessor attachment-content))
  (:metaclass mito:dao-table-mixin)
  (:unique-keys file-key))

(defun initialize-with-content (attachment content)
  (unless (and (slot-boundp attachment 'content-type)
               (slot-value attachment 'content-type))
    (etypecase content
      (pathname
       (setf (attachment-content-type attachment)
             (or (mime-lookup content) "binary/octet-stream")))
      (file-stream
       (setf (attachment-content-type attachment)
             (mime-lookup (pathname content))))
      (stream
       (setf (attachment-content-type attachment)
             "binary/octet-stream"))))

  (unless (and (slot-boundp attachment 'file-key)
               (slot-value attachment 'file-key))
    (etypecase content
      (pathname
       (setf (attachment-file-key attachment)
             (file-namestring content)))
      (file-stream
       (let ((file (pathname content)))
         (setf (attachment-file-key attachment)
               (format nil "~A~:[~;~:*.~A~]"
                       (pathname-name file)
                       (or (pathname-type file)
                           (mime-file-type (attachment-content-type attachment)))))))
      (stream
       (setf (attachment-file-key attachment)
             (format nil "~A~:[~;~:*.~A~]"
                     (uuid:print-bytes nil (uuid:make-v4-uuid))
                     (mime-file-type
                      (attachment-content-type attachment)))))))

  (unless (and (slot-boundp attachment 'file-size)
               (slot-value attachment 'file-size))
    (etypecase content
      (pathname
       (setf (attachment-file-size attachment)
             (with-open-file (in content :element-type '(unsigned-byte 8))
               (file-length in))))
      (file-stream
       (setf (attachment-file-size attachment)
             (- (file-length content) (file-position content))))
      (stream
       (setf (attachment-content attachment) (slurp-stream content))
       (setf (attachment-file-size attachment) (length content))))))

(defmethod initialize-instance :after ((attachment attachment) &rest initargs
                                       &key content &allow-other-keys)
  (declare (ignore initargs))
  (when content
    (initialize-with-content attachment content)))

(defmethod (setf attachment-content) :after (content (attachment attachment))
  (initialize-with-content attachment content))

(defgeneric attachment-file-url (attachment)
  (:method ((attachment attachment))
    (storage-file-url *storage*
                      (attachment-file-key attachment))))

(defun slurp-stream (stream &optional size)
  (if size
      (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
        (read-sequence buffer stream)
        buffer)
      (apply #'concatenate
             '(simple-array (unsigned-byte 8) (*))
             (loop with buffer = (make-array 1024 :element-type '(unsigned-byte 8))
                   for read-bytes = (read-sequence buffer stream)
                   collect (subseq buffer 0 read-bytes)
                   while (= read-bytes 1024)))))

(defmethod mito:save-dao :before ((attachment attachment))
  (when (slot-boundp attachment 'content)
    (unless (and (slot-boundp attachment 'file-key)
                 (slot-value attachment 'file-key))
      (error ":file-key is missing in ~A" attachment))
    (store-object-in-storage *storage*
                             (attachment-content attachment)
                             (attachment-file-key attachment))))

(defmethod mito:delete-dao :before ((attachment attachment))
  (delete-object-from-storage *storage*
                              (attachment-file-key attachment)))
