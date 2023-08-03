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


;; I need to write some code to get my juices flowing, and Don't have access to
;; a CLI library's docs right now.
;; So I'm just going too write a test case run-through, more or less.
;;
;; Steps:
;; - List conversations in that account
;; - List messages in a conversation
;; - List members in a conversation
;; - Post a message to a conversation
;; - Die.


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
        (loop for message in (pixie/client:messages conversation 10)
              do
              (format strm "  ~A ~A: ~A~%"
                      (pixie/client:timestamp
                        message)
                      (pixie/client:author
                        message)
                      (pixie/client:body
                        message)))
        (pixie/client:post conversation "but why")
        (pixie/client:post conversation "I decline." :in-reply-to 1)))))
