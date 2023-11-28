#+(or)
(progn
  (declaim (optimize (speed 0) (space 0) (debug 3)))
  (asdf:load-system "pixie"))

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
    (:import-from #:uiop)
    ;(:import-from #:pixie/client)
    ;(:import-from #:pixie/clients/fs)
  (:export
   main)
    )
(in-package #:pixie)

(defun stub (options name)
  (let ((strm (gethash :strm options)))
    (format strm "~A~%" name)
    (loop for k being the hash-keys of options
          using (hash-value v)
          do (format strm "~39@A: ~A~%" k v)
          finally (return (alexandria:alist-hash-table
                            `((:status . :successful)
                              (:options . ,options))
                            :test #'equal)))))

(defun whoami (options)
  (declare (type hash-table options))
  (stub options "whoami"))

(defun history (options)
  (declare (type hash-table options))
  (stub options "history"))

(defun post (options)
  (declare (type hash-table options))
  (stub options "post"))

(defun watch (options)
  (declare (type hash-table options))
  (stub options "watch"))

(defun list-conversations (options)
  (declare (type hash-table options))
  (stub options "list-conversations"))

(defun help (options)
  (declare (type hash-table options))
  (stub options "help"))

(defun main (argv &key (strm t))
  (declare (type list argv))
  (uiop:quit
    (cl-i:execute-program
      "pixie"
      (cl-i:system-environment-variables)
      `(
        (() . ,#'help)
        (("whoami") . ,#'whoami)
        (("history") . ,#'history)
        (("post") . ,#'post)
        (("watch") . ,#'watch)
        (("list-conversations") . ,#'list-conversations)
        (("help") . ,#'help)
        )
      :cli-arguments argv
      :setup (lambda (opts)
               (setf (gethash :strm opts) strm)
               opts)
      :teardown (lambda (result)
                  (format strm "~A~%"
                          (cl-i:generate-string result :pretty 4)
                          )
                  result))))
