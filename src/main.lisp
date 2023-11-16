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

#|
(defparameter *groupme-token*
  (uiop/os:getenv "GROUPME_TOKEN"))
(make-groupme-uri :path '("groups"))
#<QURI.URI.HTTP:URI-HTTPS https://api.groupme.com/v3/groups?token=<REDACTED>
QxAL9DWBg7kVtNLzaF8xtL>
* (quri:render-uri *)
"https://api.groupme.com/v3/groups?token=<REDACTED>"
|#

(defmacro with-lparallel-nursery ((channel queue &key (pool-size 10)) &body body)
   `(let ((lparallel:*kernel* (lparallel:make-kernel ,pool-size))
         (,channel (lparallel:make-channel))
         (,queue (lparallel.queue:make-queue)))
     (unwind-protect
         (progn
           ,body)
     (end-kernel lparallel:*kernel*))))

;; I need to write some code to get my juices flowing, and Don't have access to a CLI library's docs
;; right now. asdfasdf asdf adsf asdf adsf sadff asdf adsf sadf sadf asfd asdf asdf asdf fasdf asdff
;; asdfd adsf asfdd sadf sdaf adsf adsf adsf asfd asfd asfd fsda sadf sdaf adsf asfd adsf sadf adsf
;; adsf sfda fsda asfd asfd adsf adsf adsf sadf fsda fsda adsf fsda kjjV asfd asfd adsfd sdaf adsf
;; asdf adsf adsf asfd sdaf adsf asfd sdaf
;; asdf

;; So I'm just going too write a test case run-through, more or less.
;;
;; Steps:
;; - List conversations in that account
;; - List messages in a conversation
;; - List members in a conversation
;; - Post a message to a conversation
;; - watch for messages for approximately 1 minute
;; - Die.


(defun watch-print-messages (queue)
           (loop for (event status payload) = (lparallel.queue:pop-queue queue)
                 while status != :disconnect
                 do
                 (destructuring-bind (conversation message)
                     payload
                   (format strm "  ~A ~A: ~A~%"
                           (pixie/client:timestamp
                             message)
                           (pixie/client:author
                             message)
                           (pixie/client:body
                             message))))
           (receive-result channel)
           (format strm "  ~A ~A: ~A~%"
                   (pixie/client:timestamp
                     message)
                   (pixie/client:author
                     message)
                   (pixie/client:body
                     message))))

(defun main (argv &optional (strm t))
  (declare (ignore argv))
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
      (with-lparallel-nursery (channel queue)
        (lparallel:submit-task #'pixie/client:watch account
                              (lambda (item)
                                (lparallel.queue:push-queue
                                  item queue)) :timeout 60)
        (lparallel:submit-task #'watch-print-messages))

      ;; - Post a message to a conversation
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


