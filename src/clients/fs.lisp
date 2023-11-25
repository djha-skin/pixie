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
    ;(import-from #:pixie/clients/fs)
    )
(in-package #:skin.djha.pixie/client)

;(defclass dir-account
;  ((dir
;     :initarg :dir
;     :initform (error "Must specify a directory.")
;     :accessor dir
;     :type pathname)
;   (whoami
;     :initarg :whoami
;     :initform (error "Must specify a whoami.")
;     :accessor whoami
;     :type pathname)))
;
;(defmethod pixie/client:make-account ((kind :fs) specifics)
;  (pathname (gethash :directory specifics))
;  (make-instance dir-account
;                 :dir
;                 (pathname (gethash :directory specifics))
;                 (gethash :whoami specifics)
;                 ))
;
;(defmethod pixie/client:whoami ((account dir-account))
;  (whoami account))
;
;(defclass log-message
;  ((ts :initarg :ts
;       :initform (error "Need timestamp.")
;       :accessor message-ts
;       :type local-time:timestamp)
;   (author
;     :initarg :author
;     :initform (error "Need a author.")
;     :accessor message-author
;     :documentation
;     "Sender of the message."
;     :type string)
;   (in-reply-to
;     :initarg :in-reply-to
;     :accessor message-in-reply-to
;     :type log-message)
;   (body
;     :initarg :body
;     :initform (error "Need a message body.")
;     :accessor message-body
;     :documentation
;     "Body of the message."
;     :type string)))
;
;(defclass conversation-log
;  ((log-file 
;     :initarg :logfile
;     :initform (error "Must specify a log file.")
;     :accessor log-file
;     :type pathname)))
;
;(defmethod pixie/client:connect ((account dir-account))
;  "
;  This method is supposed to connect to a remote.
;  his makes _less_ sense with an FS;
;  however, the FS could theoretically be mounting a remote filesystem.
;  Thus, in order to ensure connectivity, we do a stat on the directory.
;  "
;  (probe-file (dir account)))
;
;(defmethod pixie/client:conversation-names ((account dir-account))
;  "
;  List all the log files in the directory.
;  "
;  (loop for f in
;        (uiop/filesystem:directory-files (dir account) "*.log")
;        collect (pathname-name f)))
;
;
;(defmethod pixie/client:conversation ((account dir-account) slug)
;  (declare (type string slug))
;  (make-instance
;    fs-conversation
;    :logfile (merge-pathname
;               (dir account)
;               (pathname
;                 :name
;                 slug
;                 :type
;                 "log"))))
;
;(defun find-message (seek-author seek-ts messages)
;  (declare (type seek-author string)
;           (type seek-ts string)
;           (type messages list))
;  (loop for message in messages
;        do
;        (when (and (local-time:timestamp= seek-ts (ts message))
;                   (string= seek-author (author message)))
;          (return message))
;        finally
;        return nil))
;
;(defmethod history ((conversation conversation-log) since limit)
;  (declare (type local-time:timestamp since)
;           (type (integer 0 1024) limit))
;  (with-open-file
;    (f (log-file conversation)
;       :external-format :utf-8 :direction :input)
;    (loop with messages = nil
;          for line in (readline f nil)
;          while line
;          do
;          (cl-ppcre:register-groups-bind
;            ((#'local-time:parse-timestring log-ts)
;             s
;             r
;             ra
;             (#'local-time:parse-timestring rt)
;             b)
;            ("^([^ ]+) ([^:]+): (/reply ([^:]+)@([^ ]+))? (.*)$" line)
;            (when (local-time:timestamp>= log-ts since)
;              (push messages
;                    (make-instance
;                      'log-message
;                      :ts 
;                      log-ts
;                      :in-reply-to 
;                      (when r
;                        (find-message ra rt messages))
;                      :author s
;                      :body b))))
;          finally
;          (return messages))))
;
;(defmethod pixie/client:members ((conversation conversation-log))
;  (with-open-file
;    (f (log-file conversation)
;       :external-format :utf-8 :direction :input)
;    (loop with members = nil
;          for line in (readline f nil)
;          while line
;          do
;          (cl-ppcre:register-groups-bind
;            (auth)
;            ("^[^ ]+ ([^:]+):.*$" line)
;            (pushnew auth members))
;          finally
;          (return members))))
;
;(defmethod pixie/client:author ((message log-message))
;  (author message))
;
;(defmethod pixie/client:timestamp ((message log-message))
;  (ts message))
;
;(defmethod pixie/client:body ((message log-message))
;  (body message))
;
;(defmethod post ((conversation conversation-log) body &key in-reply-to)
;            (
;             :documentation 
;             "
;             Post a message, optionally in reply to another message.
;             "
;             )
;(with-open-file
;    (f (log-file conversation)
;       :external-format :utf-8 :direction :output
;       :if-exists :append)
;    (format f 
;    (when (not (null in-reply-to))
;      (when (not (cl-ppcre:scan-to-strings "^[^:]+@[^ ]+$" in-reply-to))
;        (error "in-reply-to string is malformed."))
;      (format f "/reply ~A " in-reply-to))
;                    rt
;                    )
;
;            )
