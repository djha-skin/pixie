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

(defgeneric messages (conversation limit &key query)
            (
             :documentation
             "
             Get back a list of messages. The query is a string.
             "
             )
            )

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
