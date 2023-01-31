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

(defstruct (channel (:constructor %make-channel))
  limit
  senders
  recievers
  datas
  lock)

(defstruct active-manager
  id
  active
  lock)

(defstruct (sender (:constructor %make-sender)
		   (:include active-manager))
  data
  callback)

(defstruct (reciever (:constructor %make-reciever)
		     (:include active-manager))
  callback)

(let ((lock (bt:make-lock "cl-async-channel/atomic-counter"))
      (count 0))
  (defun get-unique-id ()
    (bt:with-lock-held (lock)
      (incf count)
      count)))

(defun make-sender (data callback lock)
  (%make-sender
   :data data
   :callback callback
   :id (get-unique-id)
   :active t
   :lock lock))

(defun make-reciever (callback lock)
  (%make-reciever
   :callback callback
   :id (get-unique-id)
   :active t
   :lock lock))


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
	    (bt:with-lock-held ((active-manager-lock head))
	      (when (active-manager-active head)
		(activate head t)
		(push-queue (sender-data head) datas))
	      t)))
	(work-recievers ()
	  (when (queue-empty-p recievers)
	    (return-from work-recievers nil))
	  (when (queue-empty-p datas)
	    (return-from work-recievers nil))
	  (let ((head (pop-queue recievers)))
	    (bt:with-lock-held ((active-manager-lock head))
	      (when (active-manager-active head)
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
		      :key #'active-manager-id)
	      (bt:with-lock-held ((active-manager-lock first))
		(bt:with-lock-held ((active-manager-lock second))
		  (when (and (active-manager-active sender)
			     (active-manager-active reciever))
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

(defun keyword-p (x name)
  (and (symbolp x)
       (string= name x)))

(defmacro select (&body clauses)
  (assert (every (lambda (clause)
		   (and (listp clause)
			(or (and (eq 4 (length (car clause)))
				 (keyword-p (caar clause) "SEND"))
			    (and (eq 3 (length (car clause)))
				 (keyword-p (caar clause) "RECV")))))
		 clauses))
  (let ((lock (gensym))
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
						  ,lock))
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
						    ,lock))
				    final-body)
				   (cons channel channels))))))
		   `(let ((,lock (bt:make-lock "cl-async-channel/select"))
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
    (with-slots (callback active) sender
      (funcall callback data)
      (setf active nil))))

(defmethod activate ((reciever reciever) data)
  (as:with-delay (0)
    (with-slots (callback active) reciever
      (funcall callback data)
      (setf active nil))))

(defmethod deactivate ((active-manager active-manager))
  (setf (active-manager-active active-manager) nil))

