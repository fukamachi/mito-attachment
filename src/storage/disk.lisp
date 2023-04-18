(in-package :cl-user)
(defpackage mito.attachment.storage.disk
  (:use #:cl
        #:mito.attachment.storage)
  (:import-from #:mito-attachment.util
                #:slurp-stream)
  (:import-from #:lack.component
                #:lack-component
                #:call)
  (:import-from #:flexi-streams
                #:make-in-memory-input-stream
                #:make-flexi-stream)
  (:import-from #:alexandria
                #:copy-stream)
  (:export #:disk-storage
           #:disk-storage-directory
           #:disk-storage-bucket-directory
           #:disk-storage-mount-path
           #:disk-storage-file))
(in-package :mito.attachment.storage.disk)

(defclass disk-storage (storage lack-component)
  ((directory :initarg :directory
              :accessor disk-storage-directory)
   (mount-path-prefix :initarg :mount-path-prefix
                      :initform nil
                      :accessor disk-storage-mount-path-prefix)))

(defgeneric disk-storage-bucket-directory (storage)
  (:method ((storage disk-storage))
    (uiop:ensure-directory-pathname
     (merge-pathnames (storage-bucket storage)
                      (slot-value storage 'directory)))))

(defgeneric disk-storage-file (storage file-key)
  (:method ((storage disk-storage) file-key)
    (merge-pathnames
     (format nil "~@[~A/~]~A" (storage-prefix storage) file-key)
     (disk-storage-bucket-directory storage))))

(defgeneric disk-storage-mount-path (storage)
  (:method ((storage disk-storage))
    (format nil "~@[~A~]/~A"
            (disk-storage-mount-path-prefix storage)
            (storage-bucket storage))))

(defmethod storage-file-url ((storage disk-storage) file-key)
  (format nil "~:[~;~:*http://~A~]~A/~A"
          (storage-endpoint storage)
          (disk-storage-mount-path storage)
          file-key))

(defmethod get-object-in-storage ((storage disk-storage) file-key)
  (with-open-file (in (disk-storage-file storage file-key) :element-type '(unsigned-byte 8))
    (flex:make-flexi-stream
      (flex:make-in-memory-input-stream
        (slurp-stream in))
      :external-format :utf-8)))

(defmethod store-object-in-storage ((storage disk-storage) (object pathname) file-key)
  (let ((file (disk-storage-file storage file-key)))
    (ensure-directories-exist file)
    (uiop:copy-file
     object
     (disk-storage-file storage file-key))))

(defmethod store-object-in-storage ((storage disk-storage) (object stream) file-key)
  (let ((file (disk-storage-file storage file-key)))
    (ensure-directories-exist file)
    (with-open-file (out file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type (stream-element-type object))
      (copy-stream object out))))

(defmethod store-object-in-storage ((storage disk-storage) (object sequence) file-key)
  (check-type object (array (unsigned-byte 8)))
  (let ((file (disk-storage-file storage file-key)))
    (ensure-directories-exist file)
    (with-open-file (out file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
      (write-sequence object out))))

(defmethod delete-object-from-storage ((storage disk-storage) file-key)
  (let ((file (disk-storage-file storage file-key)))
    (when (probe-file file)
      (delete-file file))))

(defmethod call ((storage disk-storage) env)
  (flet ((remove-slash (path)
           (if (and path
                    (< 0 (length path))
                    (char= (aref path 0) #\/))
               (subseq path 1)
               path)))
    (let* ((file-key (remove-slash (getf env :path-info)))
           (file (disk-storage-file storage file-key)))
      (if (probe-file file)
          `(200 () ,file)
          (let ((body (format nil "File not found: ~S" file-key)))
            `(404 (:content-length ,(length body)) (,body)))))))
