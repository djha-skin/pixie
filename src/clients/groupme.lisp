#+(or)
(progn
  (declaim (optimize (speed 0) (space 0) (debug 3)))
  (asdf:load-system "nrdl")
  (asdf:load-system "quri")
  (asdf:load-system "dexador"))

(in-package #:cl-user)
(defpackage
  #:skin.djha.pixie/clients/groupme (:use #:cl)
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
(in-package #:skin.djha.pixie/clients/groupme)

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

(defmethod skin.djha.pixie/client:make-client  ((kind (eql :groupme)) specifics)
  (declare (type hash-table specifics))
  (make-instance 'groupme-client
                 :api-token (gethash :api-token specifics)
                 :scheme (gethash :scheme specifics "https")
                 :host (gethash :host specifics "api.groupme.com")
                 :base-path (gethash :base-path specifics "v3")))

(defun simple-get
  (
   client
   path
   &key
   query
   )
  (multiple-value-bind
    (response code headers redir)
    (let ((q (acons "token" (api-token client) query)))
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
        (let ((unwrapped (with-input-from-string (strm response)
                          (nrdl:parse-from strm))))
          (gethash "response" unwrapped)))))

(defun adjoin-entity
  (nick id ids)
  "
  This function collapses all known nicknames for a given set of ids,
  choosing the longest nickname for each entity id.
  This helps to build a mapping between ids and nicknames.
  "
  (let ((prior (assoc id ids)))
    (when (or (not prior)
            (< (length (cdr prior))
               (length nick)))
      (acons id nick ids))))

(defun insert-by-name (nick id mapping)
  "
  Insert a `'<nick>' -> id` mapping into a
  larger mapping from nicknames to ids.

  If the nick already exists, attempt to insert '<nick> 1'.
  If that exists, attempt to insert '<nick> 2', and so forth.
  If the id is already present in the mapping under one of the
  attempted nicknames, then do nothing.
  "
  (loop
    for i = 1 then (+ i 1)
    for tried = nick then (format nil "~A ~A" nick i)
    for prior-id = (gethash tried mapping)
    while prior-id
    do
    (when (equal prior-id id)
      (return))
    finally
    (setf
      (gethash tried mapping)
      id))
  mapping)

(defun names-ids (response)
  "
  Return a hash table mapping nicknames to ids given the API response.
  "
  (let ((collapsed-nicks
          (loop for record in response
                for nick = (gethash "name" record)
                for id = (gethash "id" record)
                with accumulate = nil
                do
                (adjoin-entity nick id accumulate)
                finally
                (return 
                  (alexandira:alist-hash-table
                    (sort accumulate (lambda (a b)
                                       ; Sort based on lexicographic sort
                                       ; of nicknames.
                                       (string< (cdr a) (cdr b)))))))))
    (loop for id being the hash-keys of collapsed-nicks
          using (hash-value nick)
          with accumulate = (make-hash-table :test #'equal)
          do
          (insert-by-name
            nick
            id
            accumulate)
          finally
          (return accumulate))))

(defmethod skin.djha.pixie/client:whoami
    ((client groupme-client))
  (simple-get client
              '("users" "me")))

(defmethod skin.djha.pixie/client:room-names
    ((client groupme-client))
  (names-ids (groupme-get '("groups")
                          :query '(("omit" . "memberships")))))
    
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

(defun groupme-get
  (client path &key query)
  (loop with accumulate = (make-array 10 :fill-pointer t)
        for page = 1 then (+ page 1)
        for response = (simple-get client path (acons "page" page query))
        while (> (length response) 0)
        do
        (loop for msg across response
              do
              (vector-push-extend msg accumulate))
        finally
        (return (coerce accumulate 'list))))





;(defun ids-names
;  (ids)
;  ; we have to sort first, to ensure determinism.
;  (loop with r = (nrdl:nested-to-alist ids)
;        with r = (sort r #'string< :key #'car)
;        with building = (make-hash-table :test #'equal)
;        for (id . nick) in r
;        do
;        (insert-by-name nick id building)
;        finally
;        (return building)))
;
;
;(defmethod skin.djha.pixie/client:rooms ((client groupme-client))
;  (loop with ids = (make-hash-table :test #'equal)
;        with groups = (groupme-get :path '("groups")
;                                   :query '(("omit" . "memberships")))
;        for g across groups
;        do
;        (adjoin-entity (gethash "name" g) (gethash "id" g) ids)
;        finally
;        (return ids)))
;
;(defmethod skin.djha.pixie/client:contacts ((client groupme-client))
;    ; The past chats method is better
;    (loop with ids = (make-hash-table :test #'equal)
;          for c across (groupme-get :path '("chats"))
;          do
;          (let ((other-user (gethash "other_user" g)))
;            (adjoin-entity (gethash "name" other-user)
;                           (gethash "id" other-user)
;                           ids))
;          finally
;          (return ids)))
;
;(defmethod skin.djha.pixie/client:history ((client groupme-client)
;                                           ent
;                                           &key
;                                           limit since until)
;
