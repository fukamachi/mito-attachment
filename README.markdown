# mito-attachment

The place to store files would be a problem when you intend to write a web application which allows file-uploading. These days, AWS S3 is a common place to store/serve files, however, it's not easy to manage like RDBMS.

Mito-attachment provides a Mito mixin class for managing files outside of RDBMS. It stores files before `mito:save-dao` and deletes them before `mito:delete-dao`.

Besides, the backend storage can be replaced easily. This makes it easy that using cloud storage services for production environment and using local filesystem for development environment.

## Usage

### Setup storage

```common-lisp
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

```

### Attachment Mito class

```common-lisp
;; Attachment class for saving metadata into RDBMS
(defclass image (attachment) ()
  (:metaclass mito:dao-table-class))
```

### Save

```common-lisp
(mito:create-dao 'image
                 :file-key "file.png"
                 :content #P"uploaded-file.png"
                 :content-type "image/png")
```

See [example.lisp](example.lisp) to see the full example. It's a Lack web application which allows users to upload image files.

## Installation

```common-lisp
(ql:quickload :mito-attachment)
```

## See Also

* [Mito](https://github.com/fukamachi/mito)
* [Clipper](https://github.com/Rudolph-Miller/clipper) for Integral

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
