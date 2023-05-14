#+(or)
(declaim (optimize (speed 0) (space 0) (debug 3)))
(in-package #:cl-user)
(defpackage
  #:pixie (:use #:cl)
  (:documentation
    "
    Pixie is a small, mischievous chat client.

    It allows the user to work from the CLI with modern chat services.
    ")

    (:import-from #:alexandria)
    (:import-from #:dexador)
    (:import-from #:quri)
    (:import-from #:nrdl)
  (:export
    main)
    )
(in-package #:pixie)

(defparameter *groupme-token*
  (uiop/os:getenv "GROUPME_TOKEN"))
#|
(make-groupme-uri :path '("groups"))
#<QURI.URI.HTTP:URI-HTTPS https://api.groupme.com/v3/groups?token=<REDACTED>
QxAL9DWBg7kVtNLzaF8xtL>
* (quri:render-uri *)
"https://api.groupme.com/v3/groups?token=<REDACTED>"
|#

(defun groupme-get-paginated
  (&key
    path
    query
    (page 1)
    (scheme "https")
    (host "api.groupme.com")
    (base-path "v3")
    (token *groupme-token*))
  (multiple-value-bind
    (response code headers redir)
    (dexador:get
      (let* ((q (acons "token" token query))
             (q (acons "page" page q)))
        (quri:make-uri
          :scheme scheme
          :host host
          :path (format nil "/~A~{/~A~}"
                        base-path
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
;;      (gethash "response" (nrdl:parse-from   response)))))

(defun groupme-get
  (&rest groupme-get-paginated-args)

  (apply
    #'concatenate
    (cons 'vector
  (loop for page = 1 then (+ page 1)
        for response = (apply
                         #'groupme-get-paginated
                            (concatenate
                              'list
                              `(:page ,page)
                              groupme-get-paginated-args))
        while (> (length response) 0)
        collect response))))

(defun adjoin-entity
  (nick id ids)
  (let ((prior (gethash id ids)))
    (when (or (not prior)
            (> (length prior)
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
  (loop with building = (make-hash-table :test #'equal)
        for id being the hash-keys of ids
        using (hash-value nick)
        do
        (insert-by-name nick id building)
        finally
        (return building)))


(defun rooms ()
  "
  Returns a hash map from something the user can type out to room IDs.
  "
  (let ((ids (make-hash-table :test #'equal)))
    (loop for g across (groupme-get :path '("groups")
                                    :query '(("omit" . "memberships")))
          do
          (adjoin-entity (gethash "name" g) (gethash "id" g) ids)
          finally
          (return (ids-names ids)))))

(defun contacts ()
  "
  Returns a hash map from something the user can type out to contact IDs.
  "
    (loop with result = (make-hash-table :test #'equal)
          for g across (groupme-get :path '("groups"))
          do
          (loop for m in (gethash "members" g)
                do
                (adjoin-entity (gethash "nickname" m)
                               (gethash "user_id" m)
                                result))

          finally
          (return
            (ids-names result))))
    result))
