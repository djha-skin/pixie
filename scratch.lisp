
;;;; Create a function that closely follows the nursery parallelization pattern.
;;(defun with-lparallel-nursery (tasks)
;;  (let ((pool-size (length tasks))
;;        (lparallel:*kernel* (lparallel:make-kernel pool-size))
;;        (lparallel:*debug-tasks-p* nil)
;;        (channel (lparallel:make-channel)))
;;    (unwind-protect
;;        (loop for (name (task &rest args)) in tasks
;;              do
;;              (lparallel:submit-task channel
;;                                     (lambda ()
;;                                       (cons
;;                                         name
;;                                       (multiple-value-list
;;                                         (apply (function task) args))))))
;;      (loop for i from 0 below (length tasks)
;;            collect (receive-result channel))
;;      (end-kernel lparallel:*kernel*))))
;;
;;(let ((a (unwind-protect
;;    (+ 1 2)
;;  (print "hi"))))
;;  (format nil "A is ~A" a))
;;
;;
;;(defmacro with-lparallel-nursery ((queue &key tasks) &body body)
;;  (let ((pool-size (length tasks))
;;        (channel (gensym "channel"))
;;        (quoted-tasks (loop for task in tasks
;;                            collect (cons `(function ,(first task))
;;                                          (rest task)))))
;;    `(let ((lparallel:*kernel* (lparallel:make-kernel ,pool-size))
;;           (lparallel:*debug-tasks-p* nil)
;;           (,channel (lparallel:make-channel))
;;           (,queue (lparallel.queue:make-queue)))
;;       (unwind-protect
;;           (progn
;;             ,@(loop for task in quoted-tasks
;;                     collect
;;                     `(lparallel:submit-task ,channel
;;                                             ,@task))
;;             ,@(loop for task in quoted-tasks
;;                     collect
;;                     `(lparallel:receive-result ,channel)))
;;         (end-kernel lparallel:*kernel*))))
;;
;;   `(let ((lparallel:*kernel* (lparallel:make-kernel ,pool-size))
;;
;;         (,queue (lparallel.queue:make-queue)))
;;
;;
;;   `(let ((lparallel:*kernel* (lparallel:make-kernel ,pool-size))
;;         (,channel (lparallel:make-channel))
;;         (,queue (lparallel.queue:make-queue)))
;;     (unwind-protect
;;         (progn
;;           ,body)
;;     (end-kernel lparallel:*kernel*))))


;; So I'm just going to write a test case run-through, more or less.
;;
;; Steps:
;; - List conversations in that account
;; - List messages in a conversation
;; - List members in a conversation
;; - Post a message to a conversation
;; - watch for messages for approximately 1 minute
;; - Die.

(defun watch-print-messages (queue)
           (loop for (event status payload) = (lparallel.queue:pop-queue queue)
                 while status != :disconnect
                 do
                 (destructuring-bind (conversation message)
                     payload
                   (format strm "  ~A ~A: ~A~%"
                           (pixie/client:timestamp
                             message)
                           (pixie/client:author
                             message)
                           (pixie/client:body
                             message))))
           (receive-result channel)
           (format strm "  ~A ~A: ~A~%"
                   (pixie/client:timestamp
                     message)
                   (pixie/client:author
                     message)
                   (pixie/client:body
                     message))))

         (defun main (argv &optional (strm t))
  (declare (ignore argv))
  (let* ((config (with-open-file (in
                                  #P"./test/data/prototype-config.nrdl"
                                  :direction :input
                                  :external-format :utf-8)
                  (nrdl:parse-from in)))
        (client (pixie/client:make-client config)))
    ;; - List known accounts
    (format strm "Accounts:~%~{  - ~A ~%~}"
            (pixie/client:account-names client))
    (let ((account (pixie/client:account client "tester")))
      ;; - Login to _an_ account
      (pixie/client:connect account)
      ;; - List conversations
      (format strm "Conversations in tester:~%~{  - ~A~%~}"
              (pixie/client:conversation-names account))
      (let ((conversation (pixie/client:conversation
                            account
                            #p"~/likeaboss.txt")))
        (format strm "Conversation members for `~A`:"
                (pixie/client:members conversation))
        (format strm "Messages in ~A:~%")
        (loop for message in (pixie/client:history
                               conversation
                               (local-time:timestamp-
                                 (local-time:now)
                                 :day
                                 1)
                               10)
              do
              (format strm "  ~A ~A: ~A~%"
                      (pixie/client:timestamp
                        message)
                      (pixie/client:author
                        message)
                      (pixie/client:body
                        message)))
        (pixie/client:post conversation "but why")
        (pixie/client:post conversation "I decline." :in-reply-to "bee@2023-08-06T12:00:00-0600"))
      ;; Messages can just keep coming in. It's a web socket.
      ;; That web socket needs to stay open.
      ;; I can't make this synchronous unless I am the one opening the socket.



      (with-lparallel-nursery (queue
                        :tasks
                        (list

                        (pixie/client:watch account (lambda (message)
                                                      (lparallel.queue:push-queue
                                                        message queue)) :timeout 60)
                        (watch-print-messages queue))
                                ;;

                                (

                                 (lparallel:submit-task channel #'pixie/client:watch account
                                                        (lambda (item)
                                                          (lparallel.queue:push-queue
                                                            item queue)) :timeout 60)
                                 (lparallel:submit-task channel #'watch-print-messages)) - Post a message to a conversation
                                ;; - Die.
                                (pixie/client:disconnect account)
                                (pixie/client:stop client))
     




     (lparallel:submit-task #'pixie/client:st


        
      (pixe/client:watch account 300 (lambda (message)
                                       (format strm "  ~A ~A: ~A~%"
                                               (pixie/client:timestamp
                                                 message)
                                               (pixie/client:author
                                                 message)
                                               (pixie/client:body
                                                 message))))
      ;; with watch , i want to say,
      ;; hey, watch, cool, but only until i tell you
      ;; to stop. If you see something, do _x_ and tell me you did it.
      ;; I think the above description is relatively complete. 
      ;;

      ;; So, make a thread. in that thread, i'll do stuff and everytime I do, I'll signal the calling thread with "I did something." ??? Or, I'll just submit a job. But a producer/consumer queue is a bit heavyweight for this.
      (pixie/client:with-events
        (conversation message)
        (account 10)
