(defpackage :cl-async-channel/debug
  (:use :cl)
  (:import-from
   :cl-async-channel
   :make-channel
   :recv
   :send
   :select))

(in-package :cl-async-channel/debug)

(defun run-examples ()
  (as:start-event-loop
   (lambda ()
     (as:delay
      (lambda ()
	(format t "Timer fired.~%"))
      :time 1)))

  (as:start-event-loop
   (lambda ()
     (let ((notif (as:make-notifier (lambda () (print "CALLED!!")))))
       (as:trigger-notifier notif)
       (as:trigger-notifier notif)
       (as:trigger-notifier notif)
       )))

  (as:with-event-loop ()
    (let ((c (make-channel 0)))
      (labels ((producer (name delay n)
		 (as:with-delay (delay)
		   (when (< 0 n)
		     (send n c (lambda (tmp)
				 (declare (ignorable tmp))
				 (format t "sender ~a sended ~a~%" name n)
				 (producer name delay (1- n)))))))
	       (consumer (name delay)
		 (as:with-delay (delay)
		   (recv c (lambda (v)
			     (declare (ignorable tmp))
			     (format t "reciever ~a recieved ~a~%" name v)
			     (consumer name delay))))))
	(producer "p1" 0 10)
	(consumer "c1" 1)
	(consumer "c2" 2)
	(consumer "c3" 3))))

  (as:with-event-loop ()
    (let ((c1 (make-channel 0))
	  (c2 (make-channel 0)))
      (labels ((producer (c name delay n)
		 (as:with-delay (delay)
		   (when (< 0 n)
		     (send n c (lambda (tmp)
				 (declare (ignorable tmp))
				 (format t "sender sended ~a to ~a~%" n name)
				 (producer c name delay (1- n)))))))
	       (reciever (name delay)
		 (as:with-delay (delay)
		   (select
		     ((recv c1 x)
		      (format t "reciever ~a recieved ~a from c1~%" name x)
		      (reciever name delay))
		     ((recv c2 x)
		      (format t "reciever ~a recieved ~a from c2~%" name x)
		      (reciever name delay))))))
	(terpri)
	(terpri)
	(producer c1 "c1" 1 4)
	(producer c2 "c2" 2 4)
	(reciever "x" 1)
	(reciever "y" 1)))))
