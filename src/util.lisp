(defpackage :cl-async-channel/util
  (:use :cl)
  (:export callback-lambda))

(in-package :cl-async-channel/util)

(defmacro callback-lambda ((argument) &body body)
  (let ((called (gensym))
	(lock (gensym))
	(callback (gensym))
	(lambda-argument (gensym)))
    `(let* ((,argument nil)
	    (,called nil)
	    (,lock (bt:make-lock "callback-lambda"))
	    (,callback (as:make-notifier (lambda () ,@body))))
       (lambda (,lambda-argument)
	 (bt:with-lock-held (,lock)
	   (unless ,called
	     (setf ,called t)
	     (setf ,argument ,lambda-argument)
	     (as:trigger-notifier ,callback)))))))

