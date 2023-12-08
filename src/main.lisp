#+(or)
(progn
  (declaim (optimize (speed 0) (space 0) (debug 3)))
  (asdf:load-system "pixie"))

(in-package #:cl-user)
(defpackage
  #:skin.djha.pixie (:use #:cl)
  (:documentation
    "
    Pixie is a small, mischievous chat client.

    It allows the user to work from the CLI with modern chat services.
    "
    )

    (:import-from #:nrdl)
    (:import-from #:uiop)
    (:import-from #:skin.djha.pixie/client)
    (:import-from #:skin.djha.pixie/clients/groupme)
  (:export
   main
   entrypoint)
    )
(in-package #:skin.djha.pixie)

(defun stub (options name)
  (let ((strm (gethash :strm options)))
    (format strm "~A~%" name)
    (loop for k being the hash-keys of options
          using (hash-value v)
          do (format strm "~39@A: ~A~%" k v)
          finally (return (alexandria:alist-hash-table
                            `((:status . :successful)
                              (:options . ,options))
                            :test #'equal)))))

(defun account-list (options)
  (declare (type hash-table options))
  (alexandria:alist-hash-table `((:status . :successful)
                                 (:accounts .
                                  ,(skin.djha.pixie/client:account-names
                                     (skin.djha.pixie/client:make-client-system
                                       options))))
                               :test #'equal))

(defun client-operation (options opspec op)
  (declare (type hash-table options)
           (type function op))
  (if (null (gethash :account options))
      (alexandria:alist-hash-table
        `((:status . :cl-usage-error)
          (:options . ,options)
          (:error . "No account specified"))
        :test #'equal)
      (let ((client (skin.djha.pixie/client:make-client-system options)))
        (alexandria:alist-hash-table
          `((:status . :successful)
            (opspec .
             ,(funcall
                op
                (skin.djha.pixie/client:account
                  client
                  (gethash :account options)))))
          :test #'equal))))

(defun whoami (options)
  (declare (type hash-table options))
    (client-operation options :whoami #'skin.djha.pixie/client:whoami))

(defun history (options)
  (declare (type hash-table options))
  (stub options "history"))

(defun post (options)
  (declare (type hash-table options))
  (stub options "post"))

(defun watch (options)
  (declare (type hash-table options))
  (stub options "watch"))

(defun room-list (options)
  (declare (type hash-table options))
  (client-operation options :rooms #'skin.djha.pixie/client:room-names))

(defun help (options)
  (declare (type hash-table options))
  (stub options "help"))

(defun main (argv &key (strm t))
  "
  The functional entrypoint of the pixie command.
  "
  (declare (type list argv))
    (cl-i:execute-program
      "pixie"
      (cl-i:system-environment-variables)
      `(
        (() . ,#'help)
        (("whoami") . ,#'whoami)
        (("account" "list") . ,#'account-list)
        (("history") . ,#'history)
        (("post") . ,#'post)
        (("watch") . ,#'watch)
        (("room" "list") . ,#'room-list)
        (("help") . ,#'help)
        )
      :cli-arguments argv
      :setup (lambda (opts)
               (setf (gethash :strm opts) strm)
               opts)
      :teardown (lambda (result)
                  (format strm "~A~%"
                          (cl-i:generate-string result :pretty 4)
                          )
                  result)))

;(skin.djha.pixie:main '("whoami" "--set-account" "djhaskin987"))
;(skin.djha.pixie:main '("account" "list"))
;; TODO: Aggregate response, deal with symbols.
;(skin.djha.pixie:main '("room" "list" "--set-account" "djhaskin987"))
(defun entrypoint (argv)
  (uiop:quit
    (main argv :strm *standard-output*)))

