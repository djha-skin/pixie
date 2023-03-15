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

(defun help-page
  (options)
  "
  Print a help page for Pixie.
  "
  (format
    *standard-error*
    (cl-i:join-lines
      "Usage: pixie <subcommands>"
      ""
      "  Subcommands:"
      ""
      "    - `messages list`"
      ""
      "To see the help page for a subcommand, run `pixie <subcmd> help."
      ""
      "Given options:"
      ""
      (format nil "  ~A"
              (alexandria:alist-hash-table
                options
                :test #'equal))
      ""
      ))
  (alexandria:alist-hash-table
    '((:status . :successful))))

(defun cli-list-rooms
  (options)
  "
  List rooms in the account.
  "

  (let* ((client (make-chat-client (gethash :account options)))
         ))
    (handler-case
      (rooms (chat-client:rooms))
      ('bad-auth (this))
      ('io-error (that))))



    (alexandria:alist-hash-table
    (client-side:list-messages client 
  

(defun 

(defun main ()
  (multiple-value-bind (code outcome)
    (cl-i:execute-program
      "pixie"
      (uiop/os:env-hash-table)
      `((nil . ,#'help-page)
      (("messages" "list") . ,#'list-messages))
      :cli-arguments
      (uiop/os:get-args))
    (format t "~A~%"
            (cl-i:generate-string outcome :pretty true))
    (uiop/os:exit code)))



