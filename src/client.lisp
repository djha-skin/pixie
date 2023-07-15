#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage
  #:skin.djha.pixie/client (:use #:cl)
  (:documentation
    "
    This is the chat client interface that pixie uses to communicate with various
    chat APIs.
    ")
    (import-from #:pixie/clients/fs)
  (:export
    contacts
    rooms
    )
    )
(in-package #:skin.djha.pixie/client)

(defun make-client (config)
  )

(defun accounts (client)
  )

(defun account (client slug)
  )

(defgeneric setup (account)
            (
             :documentation
             "
             Refresh login credentials, etc.
             "
             )
            )
(defgeneric name (thing)
            (
             :documentation
             "
             Give me a name.
             "
             )
            )

(defgeneric conversations (account)
            (
             :documentation
             "
             Return a list of featured conversations.
             "
             ))

(defgeneric conversation (account key)
            (
             :documentation
             "
             Look up a conversation.
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
             Get back a list of messages.
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
