#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage
  #:skin.djha.pixie/client (:use #:cl)
  (:documentation
    "
    This is the chat client interface that pixie uses to communicate with
    various chat APIs.
    ")
    ;(import-from #:pixie/clients/fs)
  (:export
    make-client-system
    make-client
    account-names
    account
    whoami
    room-names
    ;conversations
    ;connect
    ;name
    ;conversation-names
    ;conversation
    ;members
    ;messages
    ;author
    ;timestamp
    ;body
    ;post
    ))

(in-package #:skin.djha.pixie/client)

(defgeneric make-client (kind specifics)
            (
             :documentation
             "
             Make an account object.
             "
             )
            )

(defgeneric whoami (account)
            (
             :documentation
             "
             Return the slug of the author of the client's messages.
             "
             )
            )

(defgeneric room-names (account)
            (
             :documentation
             "
             Return a list of room names.
             "
             )
            )

(defclass client-system ()
  ((accounts :initarg :accounts
             :initform (error "Must specify accounts.")
             :accessor accounts
             :type hash-table)))

(defun make-client-system (config)
  (let ((account-objects (make-hash-table :test #'equal)))
    (loop for slug being the hash-keys of (gethash :accounts config)
          using (hash-value payload)
          do
          (if (> (hash-table-count payload) 1)
              (error "Too many kinds.")
              (loop for kind being the hash-keys of payload
                    using (hash-value specifics)
                    do
                    (setf (gethash slug account-objects)
                          (skin.djha.pixie/client:make-client
                            kind
                            specifics)))))
  (make-instance 'client-system :accounts account-objects)))

(defun account-names (client)
  (loop for acc being the hash-keys of (accounts client)
        collect acc))

(defun account (client slug)
  (gethash slug (accounts client)))

;(defgeneric conversations (account)
;            (
;             :documentation
;             "
;             Return a list of featured conversations.
;             "
;             ))
;

;(defgeneric connect (account)
;            (
;             :documentation
;             "
;             Refresh login credentials, etc.
;
;             It returns a generalized boolean, whether connection worked or
;             not.
;             "
;             )
;            )
;
;(defgeneric conversation (account slug)
;            (
;             :documentation
;             "
;             Look up a conversation based on a string.
;             "
;             )
;            )
;
;(defgeneric members (conversation)
;            (
;             :documentation
;             "
;             Get back a list of current members of a conversation.
;             "
;             )
;            )
;
;;; We used to have one function, `messages`,
;;; but that turns querying into its own language.
;;; Surely we can cut corners here.
;;; See, there are some distinct use cases:
;;; We support the following questions:
;;; - What has been going on since last I came? a since timestamp.
;;; - What happened on Saturday? a since AND a before timestamp.
;;; - What did that guy say on that one time? author and search string.
;;; These are OR clauses. but there's always a limit, so we dont' really need
;;; a `before` clause.
;;;
;;; so there's really only two queries here: Search, and history.
;;; Since we're supporting only two things, we should have two functions.
;(defgeneric history (conversation since limit)
;            (
;             :documentation
;             "
;             Get back a list of messages. The parameter `since` is a timestamp.
;             "
;             )
;            )
;
;(defgeneric seek (account query)
;            (
;             :documentation
;             "
;             Search for a string in all the conversations.
;             "
;             )
;            )
;
;(defgeneric author (message)
;            (
;             :documentation
;             "
;             Get back the author of this message.
;             "
;             )
;            )
;
;(defgeneric timestamp (message)
;            (
;             :documentation
;             "
;             Get back the timestamp of this message.
;             "
;             )
;            )
;
;(defgeneric replies (message)
;            (
;             :documentation
;             "
;             Get the replies of a message.
;             "
;             )
;            )
;
;(defgeneric body (message)
;            (
;             :documentation
;             "
;             Get back the body of this message.
;             "
;             )
;            )
;
;(defgeneric post (conversation body &key in-reply-to)
;            (
;             :documentation
;             "
;             Post a message, optionally in reply to another message.
;             "
;             )
;            )
;
;
;(defgeneric watch (account callback &key filters timeout)
;  (
;   :documentation
;   "
;   Watch an acccount for incoming messages. This function is designed to work
;   well in threaded or non-threaded settings.
;
;   The function continues to watch, calling the callback whenever a message
;   comes in. If the callback returns a value other than nil, `watch` will return
;   the return value of the callback as its first value and :callback as its
;   second value.
;
;   If the returned value of the callback is nil, the function will never return
;   unless a timeout is specified. The timeout is advisory; `watch` may
;   return after the timeout due to implementation details, but should return
;   as soon as possible after the timeout expiry is noticed.
;   It returns `NIL` as its first value and `:timeout` as its second value.
;
;   The function `watch` must respond to the `stop-watching` signal (or
;   conditions subclassed thereto) by returning nil as its first value and
;   `:signal` as its second value.
;
;   Filters is an alist of filters to apply to the messages.
;   "
;  ))
