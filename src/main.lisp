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

(defun
  execute-program
  (program-name
    environment-variables
    functions
    &optional &key
    cli-arguments
    cli-aliases
    defaults
    (setup #'identity)
    (teardown #'identity)
    root-path
    reference-file
    environment-aliases
    (list-sep ",")
    (map-sep "=")
    (str-hash-init-args
      `(:test ,#'equal))
    kw-hash-init-args)
  (declare (type string program-name)
           (type hash-table environment-variables)
           (type list functions)
           (type list cli-arguments)
           (type list cli-aliases)
           (type list environment-aliases)
           (type list str-hash-init-args)
           (type list kw-hash-init-args)
           (type (or null pathname) root-path)
           (type (or null pathname) reference-file)
           (type list defaults)
           (type string list-sep)
           (type string map-sep)
           (type function setup)
           (type function teardown))





    (mapcar (lambda (s) (uiop:split-string s :separator "="))
            )))
           (get-environment-variables))
(get-environment-variables)

(defun main (argv &optional (strm t))
  (declare (type list argv))
    (cl-i:execute-program


  
  (let* ((config (with-open-file (in
                                  #P"./test/data/prototype-config.nrdl"
                                  :direction :input
                                  :external-format :utf-8)
                  (nrdl:parse-from in)))
        (client (pixie/client:make-client config)))
    ;; - List known accounts
    (format strm "Accounts:~%~{  - ~A ~%~}"
            (pixie/client:account-names client))
    (let ((account (pixie/client:account client "tester")))
      ;; - Login to _an_ account
      (pixie/client:connect account)
      ;; - List conversations
      (format strm "Conversations in tester:~%~{  - ~A~%~}"
              (pixie/client:conversation-names account))
      (let ((conversation (pixie/client:conversation
                            account
                            #p"~/likeaboss.txt")))
        (format strm "Conversation members for `~A`:"
                (pixie/client:members conversation))
        (format strm "Messages in ~A:~%")
        (loop for message in (pixie/client:history
                               conversation
                               (local-time:timestamp-
                                 (local-time:now)
                                 :day
                                 1)
                               10)
              do
              (format strm "  ~A ~A: ~A~%"
                      (pixie/client:timestamp
                        message)
                      (pixie/client:author
                        message)
                      (pixie/client:body
                        message)))
        (pixie/client:post conversation "but why")
        (pixie/client:post conversation "I decline." :in-reply-to "bee@2023-08-06T12:00:00-0600"))
      ;; Messages can just keep coming in. It's a web socket.
      ;; That web socket needs to stay open.
      ;; I can't make this synchronous unless I am the one opening the socket.



      (with-lparallel-nursery (queue
                        :tasks
                        (list

                        (pixie/client:watch account (lambda (message)
                                                      (lparallel.queue:push-queue
                                                        message queue)) :timeout 60)
                        (watch-print-messages queue))
                                ;;

                                (

                                 (lparallel:submit-task channel #'pixie/client:watch account
                                                        (lambda (item)
                                                          (lparallel.queue:push-queue
                                                            item queue)) :timeout 60)
                                 (lparallel:submit-task channel #'watch-print-messages)) - Post a message to a conversation
                                ;; - Die.
                                (pixie/client:disconnect account)
                                (pixie/client:stop client))
     




     (lparallel:submit-task #'pixie/client:st


        
      (pixe/client:watch account 300 (lambda (message)
                                       (format strm "  ~A ~A: ~A~%"
                                               (pixie/client:timestamp
                                                 message)
                                               (pixie/client:author
                                                 message)
                                               (pixie/client:body
                                                 message))))
      ;; with watch , i want to say,
      ;; hey, watch, cool, but only until i tell you
      ;; to stop. If you see something, do _x_ and tell me you did it.
      ;; I think the above description is relatively complete. 
      ;;

      ;; So, make a thread. in that thread, i'll do stuff and everytime I do, I'll signal the calling thread with "I did something." ??? Or, I'll just submit a job. But a producer/consumer queue is a bit heavyweight for this.
      (pixie/client:with-events
        (conversation message)
        (account 10)


