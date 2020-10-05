(defpackage #:mito-attachment.util
  (:use #:cl)
  (:export #:slurp-stream))
(in-package #:mito-attachment.util)

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
