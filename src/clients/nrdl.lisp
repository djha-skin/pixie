#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage
  #:skin.djha.pixie/client/nrdl (:use #:cl)
  (:documentation
    "
    This is the ''mock'' chat client in Pixie, used for testing.
    ")
    (:import-from #:nrdl)
    )
(in-package #:skin.djha.pixie/client/nrdl)

(defclass nrdl-client ()
  ((base-path
    :reader base-path
    :initarg :base-path
    :initform (error "Must specify a base path.")
    :type pathname))
  (
   :documentation
   "
   The NRDL client takes a directory. In that directory
   are expected two directories: `users` and `groups`.
   In `users`, each `<id>.nrdl` file is used to describe
   the total interaction between the current user (as defined by `id -u`)
   and person of id <id>. Example:

   {
      id: <id> [must match file name]
      display-name: <display-name>
      chats: [
              {
              timestamp: <timestamp>
              author: <author-id>
              message: <message>
              }
              ...
              ]
   }

   In groups, we have similar files, each a group. These are the same, except
   they have a `members` field as well.
   "
   ))

(defgeneric contacts (client)
            (:documentation
              "
              Table of ids to contacts.
              "
              )
            (with-open-file 


            )
