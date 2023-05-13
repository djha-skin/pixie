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
    (:import-from #:com.inuoe.jzon)
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
      (gethash "response" (com.inuoe.jzon:parse response)))))

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

(defun groups ()
  (let ((result (make-hash-table :test #'equal)))
    (loop for g across (groupme-get :path '("groups"))
          do
          (setf (gethash (gethash "name" g) result)
                (gethash "id" g)))
    result))

(defun group-messages (id back-til
  (
  (

(defun 



