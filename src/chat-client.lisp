#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage
  #:skin.djha.pixie/client (:use #:cl)
  (:documentation
    "
    This is the chat client class that pixie uses to communicate with various
    chat APIs.
    ")
    (:import-from #:dexador
                  #:com.inuoe.jzon)
  (:export
    request
    mute
    conversations
    history
    dms
    rooms
    )
    )
(in-package #:skin.djha.pixie/client)


(defgeneric request (client person)
            :documentation
            "
            Request the ability to chat with a person.
            Not needed on all platforms, thus a no-op.
            "
            )

(defgeneric mute (client person)
            :documentation
            "
            Ignore all messages from a person, whatever room or setting in which
            the message is found.
            "
            )

(defgeneric directs (client)
            :documentation
            "
            List direct conversations of interest.
            "
            )

(defgeneric rooms (client)
            :documentation
            "
            List rooms of interest.
            "
            )

(defgeneric seek-directs (client for)
            :documentation
            "
            Search for available directs.
            "
            )

(defgeneric seek-rooms (client for)
            :documentation
            "
            Search for available directs.
            "
            )

(defgeneric history (client conversation &key limit since until)
            :documentation
            "
            List <limit> messages. By default, the most recent in the given
            conversation. `since` and `until` can be used to narrow the window.
            "
            )

(defgeneric search (client for &key conversation limit)
            :documentation
            "
            Search for messages, returning the <limit> most relevant.
            "
            )

(defgeneric watch (client conversation consumer &key blacklist
                                 whitelist)
            :documentation
            "
            Forks a thread. Watches for updates in a conversation. When updates
            happen, `consumer` is callled on them.
            Returns an object that implements the `done` function below.
            Calling that function closes down that watching thread and returns
            any results from it.
            "
            )

; We have to think about watch very carefully.
; It forks a thread, and sends messages as it finds them somewhere.
; It also is constantly listening for "that's enough".
; Returns an object containing a queue and a function.
; function signals a condition.

(defgeneric done (watcher)
            :documentation
            "
            Communique informing the watcher that we're done.
            "
            )

(defgeneric post (client conversation &key in-reply-to)
            :documentation
            "
            Posts a message to a conversation, optionally in reply to another
            message.
            "
            )

(defgeneric react (client conversation message reaction)
            :documentation
            "
            Reacts to a message.
            "
            )

(defgeneric invite (client person room)
            :documentation
            "
            Invites a person into a (private) room.
            "
            )

(defgeneric kick (client person room)
            :documentation
            "
            Kicks a person from a room.
            "
            )

(defgeneric people (client room)
            :documentation
            "
            List people in a room.
            "
            )

(defgeneric rooms (client)
            :documentation
            "
            List available rooms.
            "
            )

(defgeneric join (client room)
            :documentation
            "
            Join a room.
            "
            )

(defgeneric title (client room)
            :documentation
            "
            Title or topic of the room.
            "
            )

(defgeneric (setf title) (client room title)
            :documentation
            "Set room title (or topic)."
            )

(defgeneric description (client room)
            :documentation
            "
            Room description (or MotD).
            "
            )

(defgeneric (setf description) (client room description)
            :documentation
            "
            Set room description (or MotD).
            "
            )

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

(defgeneric join-room
            (client name &rest more)
            :documentation
            "
            Join or create room.
            Returns successful or failure state, with meta information.
            ")

(defgeneric update-room
            (client name &rest more)
            :documentation
            "
            Change room
            ")


(defgeneric leave-room
              (client name)
              :documentation
              "
              Leave a room. If the room is 'owned' by the current user
              within the chat scheme, this could mean to delete it. If this is
              not possible, archive it. If that is not possible, simply leave
              it.
              "
              )

(defgeneric mute-room
            (client name)
            :documentation
            "
            Remain in the room, but do not notify for messages.
            "

(defgeneric room-members
            (client name
                    &key &optional page-size page-offset)
            :documentation
            "
            List all room members in a list.
            "
            )
