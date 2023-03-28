#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage
  #:skin.djha.pixie/chat-client (:use #:cl)
  (:documentation
    "
    This is the chat client class that pixie uses to communicate with various
    chat APIs.
    ")
    (:import-from #:dexador
                  #:com.inuoe.jzon)
  (:export
    main)
    )
(in-package #:skin.djha.pixie/chat-client)

(defclass pageable ()
  )
(defgeneric next-page (pageable)
            :documentation
            "Returns the next page of results, or <nil> or somet if there is no
            more results.
            But this is only useful if you on't want a specific page.
            "

(defclass account ()
  (name :reader name
        :initform (error "Must specify name")
        :initarg :name
        :description "Name of the account, for display")
  (id :reader id
      :initforma (error "Must specify unique ID")
      :initarg :id
      :description "ID of the account, for machine tracking"))

(defclass chat-client ()
  (account :reader account
           :initform (error "Must specify an account")
           :initarg :account
           :description "Account of this chat client"))

(defgeneric list-followed-rooms
            (client &key &optional pages-size page-offset)
            :documentation
            "
            Lists favorite rooms. Returns a list of names and optional meta
            information about rooms.
            They are the rooms typically listed on the 'buddy' list.
            Also returns how many unread messages are in the rooms, and the id
            of the last message that was read.
            "

(defgeneric list-available-rooms
            (client &key &optional page-size page-offset)
            :documentation
            "
            Lists all available rooms. Returns list of names and optional
            meta information.

            These are rooms that can be joined at any time.

            They aren't typically listed on the 'buddy' list though.
            Also returns how many unread messages are in the rooms, and the id
            of the last message that was read.
            ")
            ;; For example, the above contain "Former" rooms
            ;; on groupme.

(defgeneric create-room
            (client name &rest more)
            :documentation
            "
            Create your own room.
            Returns successful or failure state, with meta information.
            ")

(defgeneric update-room
            (client name &rest more)
            :documentation
            "
            Change room.
            ")


(defgeneric delete-room
              (client name)
              :documentation
              "
              Disavow a room. If the room is 'owned' by the current user
              within the chat scheme, this means delete it. If this is not
              possible, archive it. If that is not possible, simply
              leave it.
              "
              )

(defgeneric unfollow-room
            (client name)
            :documentation
            "
            Means different things on different clients. In the pixie model,
            stop following what is happening with this group. Leave, mute, or
            other mechanisms that don't destroy the gropu can be used,
            with precedence given to leaving, then to muting, as long
            as they can be reversed on the platform in question.
            "

(defgeneric room-members
            (client name
                    &key &optional page-size page-offset)
            :documentation
            "
            List all room members in a list.
            "
            )

(defgeneric invite-to-room
            (client name other-users)
            :documentation
            "
            On platforms that support it, invite to a room.
            If this is not supported, add that person to a room.
            If this is not supported, die.
            ")

(defgeneric room-history
            (client name
                    &key &optional since-id)
            :documentation
            "
            Get messages from room in created by descending order.
            ")

(defgeneric send-room-message
            (client destination message)
            :documentation
            "
            Send a room a message.
            ")
(defgeneric dm-history
            (client other-user &key &optional since-id)
            :documentation
            "
            Direct message history with another user.
            ")
(defgeneric post-dm
            (client other-user message)
            :documentation
            "Post a direct message to someone."
            )

(defgeneric react-to
            (client target message-id &key &optional reaction)
            :documentation
            "
            Message ID to which to react
            "

