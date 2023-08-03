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
    )
(in-package #:skin.djha.pixie/client)

(defclass dir-account
  ((dir
     :initarg :dir
     :initform (error "Must specify a directory.")
     :accessor dir
     :type pathname)
   (whoami
     :initarg :whoami
     :initform (error "Must specify a whoami.")
     :accessor whoami
     :type pathname)))



(defmethod pixie/client:make-account ((kind :fs) specifics)
  (pathname (gethash :directory specifics))
  (make-instance dir-account
                 :dir
                 (pathname (gethash :directory specifics))
                 (gethash :whoami specifics)
                 ))

(defmethod pixie/client:whoami ((account dir-account))
  (whoami account))

(defclass log-message
  ((ts :initarg :ts
       :initform (error "Need timestamp.")
       :accessor message-ts
       :type local-time:timestamp)
   (author
     :initarg :author
     :initform (error "Need a author.")
     :accessor message-author
     :documentation
     "Sender of the message."
     :type string)
   (in-reply-to
     :initarg :in-reply-to
     :accessor message-in-reply-to
     :type log-message)
   (body
     :initarg :body
     :initform (error "Need a message body.")
     :accessor message-body
     :documentation
     "Body of the message."
     :type string)))

(defclass conversation-log
  ((log-file 
     :initarg :logfile
     :initform (error "Must specify a log file.")
     :accessor log-file
     :type pathname)))

(defmethod pixie/client:connect ((account dir-account))
  "
  This method is supposed to connect to a remote.
  his makes _less_ sense with an FS;
  however, the FS could theoretically be mounting a remote filesystem.
  Thus, in order to ensure connectivity, we do a stat on the directory.
  "
  (probe-file (dir account)))

(defmethod pixie/client:conversation-names ((account dir-account))
  "
  List all the log files in the directory.
  "
  (loop for f in
        (uiop/filesystem:directory-files (dir account) "*.log")
        collect (pathname-name f)))


(defmethod pixie/client:conversation ((account dir-account) slug)
  (declare (type string slug))
  (make-instance
    fs-conversation
    :logfile (merge-pathname
               (dir account)
               (pathname
                 :name
                 slug
                 :type
                 "log"))))

(defclass log-message-seeker ()
  ((ts :initarg :ts
       :initform (error "Need timestamp.")
       :accessor matcher-ts
       :type local-time:timestamp)
   (author
     :initarg :author
     :initform (error "Need an author.")
     :accessor matcher-author
     :documentation
     "Sender of the message."
     :type string)))

;; aggregates matchers
;; query is its own language. D'oh.

(defclass compound-matcher ()
  (matchers))

(defclass since-matcher ()
  (after-ts))
;; should I even be using CLOS here.
;; what about a dictionary mapping keys to functions.
;; That seems like a better idea.
;; TODO
'(( :key . myfunc ))

(defgeneric message-matches (message matcher))

(defmethod message-matches (message (matcher null))
  message)

(defmethod message-matches (message (matcher log-message-matcher))
  (and (equal (matcher-ts matcher)
              (message-ts message))
       (equal (matcher-author matcher)
              (message-author message))
       message))

(defun find-message (messages matcher)
  (declare (type author string)
           (type ts string)
           (type messages list))
    (loop with matcher = (make-matcher query)
          for message in messages
          if (message-matches message matcher)
          collect message)
  )

(defun make-matcher (query)
  (declare (type query hash-table))

  (if (null query)
    null
    (make-instance
      'log-message-matcher
      :ts (local-time:parse-rfc3339-timestring
      (gethash :timestamp hash))
      :author (gethash :author hash))))

(defmethod pixie/client:messages ((conversation conversation-log) limit &key query)
  "
  Return messages based on a query.
  The query can have start date, end date, and reply-to, optionally.
  Queries are always a map of strings.
  "

  )



(defun make-matcher (query)
  (declare (type query hash-table))
  )


(defun fs-messages (conversation limit &key query)
  (declare (type fs-conversation conversation))
  (with-open-file
    (f (logfile fs-conversation)
       :external-format :utf-8 :direction :input)
    (loop with matcher = (when query (make-matcher query))
          with messages = nil
          for line in (readline f nil)
          while line
          do
          (cl-ppcre:register-groups-bind
            (ts s r ra rt b)
            ("^([^ ]+) ([^:]): (/reply ([^:]+)@([^ ]+))? (.*)$" line)
            (if r
              (make-instance
                    fs-message
                    :ts (local-time:parse-timestring ts)
                    :in-reply-to (find-message ra rt messages)
                    :author s
                    :body b)

            (push  messages))
          finally
          (return messages))))

(defmethod messages ((conversation fs-conversation) limit &key query)


;; TODO Finish messages in terms of fs-messages
;; TODO Finish `members` in terms of fs-messages
:; TODO Finish post in 
            (setf (gethash auth members) t))
          finally
          (return
            (loop for author being the hash-keys of members collect author)))))
  (
(defmethod pixie/client:members ((conversation fs-conversation))
  )

(defmethod pixie/client:author ((message fs-message))
  (author message))

(defmethod timestamp (message)
  (
            (
             :documentation
             "
             Get back the timestamp of this message.
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
