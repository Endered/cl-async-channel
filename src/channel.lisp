(defpackage :cl-async-channel/channel
  (:use :cl)
  (:import-from
   :cl-async-channel/queue
   :make-queue
   :queue-size
   :queue-empty-p
   :push-queue
   :pop-queue
   :peek-queue)
  (:export
   :make-channel
   :send
   :recv
   :select))

(in-package :cl-async-channel/channel)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun keyword-p (x name)
    (and (symbolp x)
	 (string= name x))))

(defstruct (channel (:constructor %make-channel))
  limit
  senders
  recievers
  datas
  lock)

(defstruct (active-manager (:constructor %make-active-manager))
  id
  active
  lock)

(defun make-active-manager ()
  (%make-active-manager
   :id (get-unique-id)
   :active t
   :lock (bt:make-lock "cl-async-channel/active-manager")))

(defstruct (sender (:constructor %make-sender))
  active-manager
  data
  callback)

(defstruct (reciever (:constructor %make-reciever))
  active-manager
  callback)

(let ((lock (bt:make-lock "cl-async-channel/atomic-counter"))
      (count 0))
  (defun get-unique-id ()
    (bt:with-lock-held (lock)
      (incf count)
      count)))

(defun make-sender (data callback active-manager)
  (%make-sender
   :data data
   :callback callback
   :active-manager active-manager))

(defun make-reciever (callback active-manager)
  (%make-reciever
   :callback callback
   :active-manager active-manager))

(defmethod get-lock ((sender sender))
  (active-manager-lock (sender-active-manager sender)))

(defmethod get-lock ((reciever reciever))
  (active-manager-lock (reciever-active-manager reciever)))

(defmethod active-p ((sender sender))
  (active-manager-active (sender-active-manager sender)))

(defmethod active-p ((reciever reciever))
  (active-manager-active (reciever-active-manager reciever)))

(defmethod get-id ((sender sender))
  (active-manager-id (sender-active-manager sender)))

(defmethod get-id ((reciever reciever))
  (active-manager-id (reciever-active-manager reciever)))

(defun make-channel (limit)
  (%make-channel
   :limit limit
   :senders (make-queue)
   :recievers (make-queue)
   :datas (make-queue)
   :lock (bt:make-lock "cl-async-channel/channel")))

(defmethod add-sender ((channel channel) (sender sender))
  (with-slots (senders lock) channel
    (bt:with-lock-held (lock)
      (push-queue sender senders))
    (refresh-channel channel)))

(defmethod add-reciever ((channel channel) (reciever reciever))
  (with-slots (recievers lock) channel
    (bt:with-lock-held (lock)
      (push-queue reciever recievers))
    (refresh-channel channel)))

(defun refresh-channel (channel)
  (uiop:nest
   (as:with-delay (0))
   (with-slots (limit senders recievers datas lock) channel)
   (bt:with-lock-held (lock))
   (labels
       ((work-senders ()
	  (when (queue-empty-p senders)
	    (return-from work-senders nil))
	  (unless (< (queue-size datas) limit)
	    (return-from work-senders nil))
	  (let ((head (pop-queue senders)))
	    (bt:with-lock-held ((get-lock head))
	      (when (active-p head)
		(activate head t)
		(push-queue (sender-data head) datas))
	      t)))
	(work-recievers ()
	  (when (queue-empty-p recievers)
	    (return-from work-recievers nil))
	  (when (queue-empty-p datas)
	    (return-from work-recievers nil))
	  (let ((head (pop-queue recievers)))
	    (bt:with-lock-held ((get-lock head))
	      (when (active-p head)
		(activate head (pop-queue datas)))
	      t)))
	(work-both ()
	  (when (queue-empty-p senders)
	    (return-from work-both nil))
	  (when (queue-empty-p recievers)
	    (return-from work-both nil))
	  (unless (queue-empty-p datas)
	    (return-from work-both nil))
	  (let ((sender (peek-queue senders))
		(reciever (peek-queue recievers)))
	    (destructuring-bind (first second)
		(sort (list sender reciever)
		      #'<
		      :key #'get-id)
	      (bt:with-lock-held ((get-lock first))
		(bt:with-lock-held ((get-lock second))
		  (when (and (active-p sender)
			     (active-p reciever))
		    (pop-queue senders)
		    (pop-queue recievers)
		    (activate sender t)
		    (activate reciever (sender-data sender))
		    t))))))
	(rec ()
	  (when (or (work-senders)
		    (work-recievers)
		    (work-both))
	    (rec))))
     (rec))))



(defmacro select (&body clauses)
  (assert (every (lambda (clause)
		   (and (listp clause)
			(or (and (eq 4 (length (car clause)))
				 (keyword-p (caar clause) "SEND"))
			    (and (eq 3 (length (car clause)))
				 (keyword-p (caar clause) "RECV")))))
		 clauses))
  (let ((active-manager (gensym))
	(refresh (gensym)))
    (labels ((rec (clauses tmp-binds final-body channels)
	       (if clauses
		   (let ((channel-op (caar clauses))
			 (body (cdar clauses)))
		     (cond ((keyword-p (car channel-op) "SEND")
			    (let ((channel (gensym))
				  (data (gensym)))
			      (rec (cdr clauses)
				   (append
				    (list
				     `(,channel ,(nth 2 channel-op))
				     `(,data ,(nth 1 channel-op)))
				    tmp-binds)
				   (cons
				    `(add-sender ,channel
						 (cl-async-channel/channel::make-sender
						  data
						  (lambda (,(nth 3 channel-op))
						    (,refresh)
						    ,@body)
						  ,active-manager))
				    final-body)
				   (cons channel channels))))
			   ((keyword-p (car channel-op) "RECV")
			    (let ((channel (gensym)))
			      (rec (cdr clauses)
				   (append
				    (list
				     `(,channel ,(nth 1 channel-op)))
				    tmp-binds)
				   (cons
				    `(add-reciever ,channel
						   (cl-async-channel/channel::make-reciever
						    (lambda (,(nth 2 channel-op))
						      (,refresh)
						      ,@body)
						    ,active-manager))
				    final-body)
				   (cons channel channels))))))
		   `(let ((,active-manager (cl-async-channel/channel::make-active-manager))
			  ,@ (reverse tmp-binds))
		      (labels ((,refresh ()
				 ,@ (mapcar (lambda (channel)
					      `(cl-async-channel/channel::refresh-channel ,channel))
					    (reverse channels))))
			,@ (reverse final-body))))))
      (rec clauses nil nil nil))))

(defmethod send (data (channel channel) callback)
  (select ((send data channel x) (funcall callback x))))

(defmethod recv ((channel channel) callback)
  (select ((recv channel x) (funcall callback x))))

(defmethod activate ((sender sender) data)
  (as:with-delay (0)
    (with-slots (callback active-manager) sender
      (setf (active-manager-active active-manager) nil)
      (funcall callback data))))

(defmethod activate ((reciever reciever) data)
  (as:with-delay (0)
    (with-slots (callback active-manager) reciever
      (setf (active-manager-active active-manager) nil)
      (funcall callback data))))

(defmethod deactivate ((active-manager active-manager))
  (setf (active-manager-active active-manager) nil))

