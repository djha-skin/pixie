#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage
  #:pixie (:use #:cl)
  (:documentation
    "
    Pixie is a small, mischievous chat client.

    It allows the user to work from the CLI with modern chat services.
    ")

    (:import-from #:nrdl)
    (:import-from #:pixie/client)
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


(defun main (argv)
  (declare (ignore argv))
  (let* ((config (with-open-file (in
                                  #P"./test/data/prototype-config.nrdl"
                                  :direction :input 
                                  :external-format :utf-8)
                  (nrdl:parse-from in)))
        (client (pixie/client:make-client config)))
    ;; - List known accounts
    (format "Accounts:~%~{  - ~A~%~}"
            (pixie/client:accounts client))
    (let ((account (pixie/client:account client "tester")))
      ;; - Login to _an_ account
      (pixie/client:setup account)
      ;; - List conversations
      (format "Conversations in ~A:~%~{  - ~A~%~}"
              (pixie/client:name account)
              (pixie/client:conversations account))
      (let ((conversation (pixie/client:conversation account #p"~/likeaboss.txt")))
        (format "Conversation members for `~A`:"
                (pixie/client:members conversation))
        (format "Messages in ~A:~%~{  ~A~%~}"
                (pixie/client:messages conversation 10))
        (pixie/client:post conversation "but why")
        (pixie/client:post conversation "I decline." :in-reply-to 1)
        )
      )))
