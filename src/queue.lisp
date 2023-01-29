(defpackage :cl-async-channel/queue
  (:use :cl)
  (:export
   :make-queue
   :queue-size
   :queue-empty-p
   :push-queue
   :pop-queue
   :peek-queue))

(in-package :cl-async-channel/queue)

(defstruct (queue (:constructor %make-queue)
		  (:conc-name %queue))
  head
  tail
  lock
  size)

(defun make-queue ()
  (%make-queue :head nil
	       :tail nil
	       :size 0
	       :lock (bt:make-lock "cl-async-channel/queue")))

(defmethod queue-empty-p ((queue queue))
  (with-slots (size lock) queue
    (bt:with-lock-held (lock)
      (zerop size))))

(defmethod queue-size ((queue queue))
  (with-slots (size lock) queue
    (bt:with-lock-held (lock)
      size)))

(defmethod peek-queue ((queue queue))
  (with-slots (head size lock) queue
    (bt:with-lock-held (lock)
      (when (zerop size)
	(error "queue must have element on peek"))
      (car head))))

(defmethod push-queue (item (queue queue))
  (with-slots (head tail size lock) queue
    (bt:with-lock-held (lock)
      (cond ((zerop size)
	     (let ((new (list item)))
	       (setf head new)
	       (setf tail new)
	       (setf size 1)))
	    (t
	     (setf (cdr tail) (list item))
	     (setf tail (cdr tail))
	     (incf size))))))

(defmethod pop-queue ((queue queue))
  (with-slots (head tail size lock) queue
    (bt:with-lock-held (lock)
      (when (zerop size)
	(error "queue must have element on pop"))
      (prog1
	  (car head)
	(cond ((eq 1 size)
	       (setf head nil)
	       (setf tail nil)
	       (setf size 0))
	      (t
	       (setf head (cdr head))
	       (decf size)))))))

