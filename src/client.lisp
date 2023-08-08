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
    (import-from #:pixie/clients/fs)
  (:export
    make-client
    account-names
    account
    connect
    name
    conversation-names
    conversation
    members
    messages
    author
    timestamp
    body
    post))

(in-package #:skin.djha.pixie/client)


(defclass root ()
  ((accounts :initarg :accounts
             :initform (error "Must specify accounts.")
             :accessor accounts
             :type hash-table)))

(defun make-client (config)
  (let ((account-objects (make-hash-table :test #'equal)))
    (loop for kind being the hash-keys of (gethash :accounts config)
          using (hash-value payload)
          do
          (loop for slug being the hash-keys of payload
                using (hash-value specifics)
                do
                (setf (gethash slug account-objects)
                      (pixie/client:make-account
                        kind
                        specifics)))))
  (make-instance root :accounts account-objects))

(defun account-names (client)
  (loop for acc being the hash-keys of (accounts client)
        collect acc))

(defun account (client slug)
  (gethash slug (accounts client)))

(defgeneric make-account (kind specifics)
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

(defgeneric connect (account)
            (
             :documentation
             "
             Refresh login credentials, etc.

             It returns a generalized boolean, whether connection worked or
             not.
             "
             )
            )

(defgeneric conversation-names (account)
            (
             :documentation
             "
             Return a list of featured conversations.
             "
             ))

(defgeneric conversation (account slug)
            (
             :documentation
             "
             Look up a conversation based on a string.
             "
             )
            )

(defgeneric members (conversation)
            (
             :documentation
             "
             Get back a list of current members of a conversation.
             "
             )
            )

;; We used to have one function, `messages`,
;; but that turns querying into its own language.
;; Surely we can cut corners here.
;; See, there are some distinct use cases:
;; We support the following questions:
;; - What has been going on since last I came? a since timestamp.
;; - What happened on Saturday? a since AND a before timestamp.
;; - What did that guy say on that one time? author and search string.
;; These are OR clauses. but there's always a limit, so we dont' really need
;; a `before` clause.
;;
;; so there's really only two queries here: Search, and history.
;; Since we're supporting only two things, we should have two functions.

(defgeneric history (conversation since limit)
            (
             :documentation
             "
             Get back a list of messages. The parameter `since` is a timestamp.
             "
             )
            )

; This is a 2.0
;(defgeneric seek (account query)
;            (
;             :documentation
;             "
;             Search for a string in all the conversations.
;             "
;             )
;            )

(defgeneric author (message)
            (
             :documentation
             "
             Get back the author of this message.
             "
             )
            )

(defgeneric timestamp (message)
            (
             :documentation
             "
             Get back the timestamp of this message.
             "
             )
            )

(defgeneric replies (message)
            (
             :documentation
             "
             Get the replies of a message.
             "
             )
            )

(defgeneric body (message)
            (
             :documentation
             "
             Get back the body of this message.
             "
             )
            )

(defgeneric post (conversation body &key in-reply-to)
            (
             :documentation
             "
             Post a message, optionally in reply to another message.
             "
             )
            )
(define-condition stop-watching
  (signal)
  ()
  (:documentation
    "
    Use this to signal to the watching thread to stop watching. This signal
    is intended to be used in a threaded context. One thread would signal the
    watching thread using this signal. If used, it will stop the watch function
    and cause it to return T, closing any necessary resources before doing so
    ")
    (:report (lambda (condition stream)
               (format stream "Requesting the watcher to stop watching."))))

(defgeneric watch (account callback)
            (
             :documentation
             "
             Watch an acccount for incoming messages.

             This function is designed to work well in threaded or non-threaded
             settings.

             The function continues to watch, calling the callback whenever a
             message comes in. Return the value of the return value of the callback,
             if it is not nil.

             If the returned value of the callback is nil, the function will
             never retur

             The function will signal an error in the usual way. It will also
             respond to the `stop-watching` signal (or conditions subclassed
             thereto). This signal will be handled by causing the function to
             return nil. A error may be signaled instead of returning nil.

             The drawback of this design is I can't force people to set a
             restart, or even discuss how that would look. I can't enforce
             the handling of the condition. Some more thought is in good order.

             How even

             So the 'even discuss how that would look' part is a code smell,
             meaning I've gotten the wrong abstraction. I'm not doing something
             right. But I think I 'm close.

             The restart/condition is a way to get it to stop. Why not let
             someone else make that part up? Let the implementer handle it.
             After all, the implementer is the one which will be signaling

             NO IT IS NOT

             I AM GOING TO SIGNAL THAT. I, THE GUI IMPLEMENTER.

             I will signal the `stop watching` on a ctrl+c or timeout.

            See, I used to have it in my head that the watch fun wouldd be
            asynchronous, returning immediately, and that the end watch would be
            similar. But I rescinded this. So, I can't just stop watching
            whenever I want. I *have* to use threading to accomplish that result, OR
            in the callback.

            Which is dumb. I don't want to _require_ threading, only accomodate it.

            I could make the callback a sort of heartbeat deal, but this is a hack.

            What an interesting design problem.
            "
                )
            )



