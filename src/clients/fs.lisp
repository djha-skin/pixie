#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage
  #:skin.djha.pixie/clients/fs (:use #:cl)
  (:documentation
    "
    This is the chat API for a file-system based chat system.
    It is largely used for testing the chat clients API.

    Files are conversations. Members are users who can see the file.
    Messages are of the form <author>: <text>. Continued text are on subsequent
    lines, prefixed by whitespace. If whitespace is found at the beginning of
    the line, the line is a continuation of the previous.
    Line number may be used as an in-reply-to address.

    ")
    (import-from #:pixie/clients/fs)
  (:export
    contacts
    rooms
    )
    )
(in-package #:skin.djha.pixie/client)

(defclass fs-account)

(defmethod setup (account)
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

(defgeneric members (conversation limit)
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
