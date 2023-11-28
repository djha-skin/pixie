#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage
  #:skin.djha.pixie/client/groupme (:use #:cl)
  (:documentation
    "
    This is the groupme chat client for Pixie.
    ")
    (:import-from #:skin.djha.pixie/client)
    (:import-from #:alexandria)
    (:import-from #:dexador)
    (:import-from #:quri)
    (:import-from #:nrdl)
    )
(in-package #:skin.djha.pixie/client/groupme)

(defclass groupme-client ()
  ((api-token
    :reader api-token
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
   (scheme
     :reader scheme
     :initarg :scheme
     :initform "https")
   (host
     :reader host
     :initarg :host
     :initform "api.groupme.com")
   (base-path
     :reader base-path
     :initarg :base-path
     :initform "v3"))
  (:documentation
    "
    GroupMe API Chat Client.
    "
    )
  )

(defmethod skin.djha.pixie/client:make-account ((kind :groupme) specifics)
  (declare (type hash-table specifics))
  (make-instance 'groupme-client
                 :api-token (gethash "api-token" specifics)
                 :scheme (gethash "scheme" specifics "https")
                 :host (gethash "host" specifics "api.groupme.com")
                 :base-path (gethash "base-path" specifics "v3")))

(defun simple-get
  (
   client
   &key
   query
   path
   )
  (multiple-value-bind
    (response code headers redir)
    (let ((q (acons "token" (token client) query)))
      (dexador:get
        (quri:make-uri
          :scheme (scheme client)
          :host (host client)
          :path (format nil "/~A~{/~A~}"
                        (base-path client)
                        path)
          :query q)))
    (declare (ignore headers)
             (ignore redir))
    (if (> code 399)
      (error "bad")
      (gethash
        "response"
        (with-input-from-string (strm response)
          (nrdl:parse-from strm))))))

(defmethod skin.djha.pixie/client:whoami
    ((client groupme-client))
    (gethash "response"
             (simple-get client
                         :path '("users" "me"))))

;(defun get-group-messages
;  (
;   client
;   id
;   (limit 10000)
;   &key
;   since
;   until
;   )
;  (declare (type groupme-client client)
;           (type list query)
;           (type integer page))
;  (loop with messages = (make-array 10 :fill-pointer t)
;        with query = `(("token" . ,(api-token client)))
;        with response = (simple-get client 
;                                    :path
;                                    (format "~{/~A~}"
;                                            `("groups"
;                                              ,id
;                                              "messages")))
;        do
;        (loop for msg across (gethash "messages")
;
;
;                                    groups/(multiple-value-bind
;                          (dexador:get
;                          (quri:make-uri
;                            :scheme (scheme client)
;                            :host (host clieent)
;                            :path (format nil "/~A~{/~A~}"
;                                          (base-path client))
;                            :query query)
;  (multiple-value-bind
;    (response code headers redir)
;      (let* ((q (acons "token" (api-token client) query))
;             (
;             (q (acons "
;        (quri:make-uri
;          :scheme (scheme client)
;          :host (host client)
;          :query q)))
;
(defun get-paginated
  (
   client
   &key
   query
   (page 1)
   )
  (declare (type groupme-client client)
           (type list query)
           (type integer page))
  (multiple-value-bind
    (response code headers redir)
    (dexador:get
      (let* ((q (acons "token" (api-token client) query))
             (q (acons "page" page q)))
        (quri:make-uri
          :scheme (scheme client)
          :host (host client)
          :path (format nil "/~A~{/~A~}"
                        (base-path client)
                        path)
          :query q)))
    (declare (ignore headers)
             (ignore redir))
    (if (> code 399)
      (error "bad")
      (gethash
        "response"
        (with-input-from-string (strm response)
          (nrdl:parse-from strm))))))

(defun groupme-get
  (&rest groupme-get-paginated-args)
  (apply
    #'concatenate
    (cons 'vector
  (loop for page = 1 then (+ page 1)
        for response = (apply
                         #'get-paginated
                            (concatenate
                              'list
                              groupme-get-paginated-args
                              `(:page ,page)))
        while (> (length response) 0)
        collect response))))

(defun adjoin-entity
  (nick id ids)
  (let ((prior (gethash id ids)))
    (when (or (not prior)
            (< (length prior)
               (length nick)))
      (setf (gethash id ids) nick))))

(defun insert-by-name (k v result)
  (loop
    for i = 1 then (+ i 1)
    for tried = k then (format nil "~A ~A" k i)
    for prior = (gethash tried result)
    while prior
    do
    (when (equal prior v)
      (return))
    finally
    (setf
      (gethash tried result)
      v))
  result)

(defun ids-names
  (ids)
  ; we have to sort first, to ensure determinism.
  (loop with r = (alexandria:hash-table-alist ids)
        with r = (sort r #'string< :key #'car)
        with building = (make-hash-table :test #'equal)
        for (id . nick) in r
        do
        (insert-by-name nick id building)
        finally
        (return building)))


(defmethod skin.djha.pixie/client:rooms ((client groupme-client))
  (loop with ids = (make-hash-table :test #'equal)
        with groups = (groupme-get :path '("groups")
                                   :query '(("omit" . "memberships")))
        for g across groups
        do
        (adjoin-entity (gethash "name" g) (gethash "id" g) ids)
        finally
        (return ids)))

(defmethod skin.djha.pixie/client:contacts ((client groupme-client))
    ; The past chats method is better
    (loop with ids = (make-hash-table :test #'equal)
          for c across (groupme-get :path '("chats"))
          do
          (let ((other-user (gethash "other_user" g)))
            (adjoin-entity (gethash "name" other-user)
                           (gethash "id" other-user)
                           ids))
          finally
          (return ids)))

(defmethod skin.djha.pixie/client:history ((client groupme-client)
                                           ent
                                           &key
                                           limit since until)

