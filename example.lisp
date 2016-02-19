;; Usage:
;;   $ clackup example.lisp --port 8080
;:   $ APP_ENV=production AWS_ACCESS_KEY=xxx AWS_SECRET_KEY=xxx clackup example.lisp --port 8080
;;
;; 'clackup' command is a Roswell script provided by Clack (http://clacklisp.org).
;; You can install it by 'ros install clack'.

(ql:quickload '(:mito-attachment :lack-request :uuid) :silent t)

(in-package :cl-user)
(defpackage mito.attachment.example
  (:use :cl
        :mito.attachment
        :lack
        :lack.request))
(in-package :mito.attachment.example)

(defvar *appenv* (uiop:getenv "APP_ENV"))

;; Setup storage class
(setf *storage*
      (if (string= *appenv* "production")
          ;; Store files in AWS S3 for production environment
          (make-instance 's3-storage
                         :bucket "mito-attachment-example"
                         :endpoint "s3-ap-northeast-1.amazonaws.com"
                         :access-key (uiop:getenv "AWS_ACCESS_KEY")
                         :secret-key (uiop:getenv "AWS_SECRET_KEY"))
          ;; Store files in local filesystem for development environment
          (make-instance 'disk-storage
                         :bucket "mito-attachment-example"
                         :directory #P"/tmp/attachment/")))

;; Attachment class for saving metadata into RDBMS
(defclass image (attachment) ()
  (:metaclass mito:dao-table-class))

;; Use SQLite3
(mito:connect-toplevel :sqlite3 :database-name #P"/tmp/attachment.db")
(mito:ensure-table-exists 'image)

;; Main Lack app
(defun app (env)
  (let ((req (make-request env)))
    (if (eq (request-method req) :post)
        (destructuring-bind (content field-meta headers)
            (cdr (assoc "file" (request-parameters req) :test #'string=))
          (mito:create-dao 'image
                           :file-key (format nil "~A.~A" (uuid:print-bytes nil (uuid:make-v4-uuid))
                                             (pathname-type (gethash "filename" field-meta)))
                           :content content
                           :content-type (gethash "content-type" headers))
          `(302 (:location "/") ("ok")))
        `(200 (:content-type "text/html")
              (,(format nil "<html>
<head>
<style>
img {
  display: inline-block;
  margin: 5px;
  max-width: 120px;
}
</style>
</head>
<body>
<div>~{<img src=\"~A\">~}</div>
<form method=\"post\" enctype=\"multipart/form-data\">
  <input type=\"file\" name=\"file\">
  <input type=\"submit\">
</form>
</body>
</html>" (mapcar #'attachment-file-url (mito:retrieve-dao 'image))))))))

(lack:builder
  :accesslog
  ;; disk-storage serves uploaded files from the same server at /attachment.
  (when (typep *storage* 'disk-storage)
    `(:mount ,(disk-storage-mount-path *storage*) ,*storage*))
  #'app)
