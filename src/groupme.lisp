#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage
  #:skin.djha.pixie/chat-clients/groupme (:use #:cl)
  (:documentation
    "
    This is the groupme chat client for Pixie.
    ")
    (:import-from #:dexador
                  #:com.inuoe.jzon)
  (:export
    main)
    )
(in-package #:skin.djha.pixie/chat-clients/groupme)

(defclass groupme-client ()
  (api-token :accessor api-token
             :initarg :api-token
             :initform (error "Must specify an API token.")
             :type string
             :documentation
             "
             The API token of the user. To obtain, go to dev.groupme.com and
             click in the upper-right-hand corner of the page after logging
             in.
             "
             )
  :documentation
  "
  GroupMe API Chat Client.
  ")




