#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage
  #:pixie (:use #:cl)
  (:documentation
    "
    Pixie is a small, mischievous chat client.

    It allows the user to work from the CLI with modern chat services.
    "
    )

    (:import-from #:nrdl)
    (:import-from #:pixie/client)
    (:import-from #:pixie/clients/fs)
  (:export
   main)
    )
(in-package #:pixie)


(defun stub (options name)
  (format strm "~A~%" name)
  (loop for (k v) being the hash-key-pairs of options
        do (format strm "~40@A: ~40A~%" k v)))

(defun history (options)
  (declare (type hash-table options))
  (stub options "history"))

(defun post (options)
  (declare (type hash-table options))
  (stub options "post"))

(defun watch (options)
  (declare (type hash-table options))
  (stub options "watch"))

(defun list (options)
  (declare (type hash-table options))
  (stub options "list"))

(defun help (options)
  (declare (type hash-table options))
  (stub options "help"))

(defun main (argv &key (strm t))
  (declare (type list argv))
  (multiple-value-bind (code result)
  (cl-i:execute-program
    "pixie"
    (cl-i:system-environment-variables)
    `(
      ("history" . ,#'history)
      ("post" . ,#'post)
      ("watch" . ,#'watch)
      ("list" . ,#'list)
      ("help" . ,#'help)
      )
    :cli-arguments argv
    :setup (lambda (opts)
             (setf (gethash :strm opts) strm))
    :teardown (lambda (opts)
                (format (gethash :strm opts) "~%")))
    (format "~A~%" result)
    (when (not (zerop code))
      (error "pixie exited with code ~A" code))))
