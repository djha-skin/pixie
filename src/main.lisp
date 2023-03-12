#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage
  #:pixie (:use #:cl)
  (:documentation
    "
    Pixie is a small, mischeivous chat client.

    It allows the user to work from the CLI with modern chat services.
    ")
    (:import-from #:cl-i)
  (:export
    main)
    )
(in-package #:pixie)

(defun main (argv)
  argv)
