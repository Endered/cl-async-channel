(defpackage :cl-async-channel
  (:use :cl)
  (:import-from
   :cl-async-channel/channel
   :make-channel
   :send
   :recv
   :select)
  (:export
   :make-channel
   :send
   :recv
   :select

   :>!
   :>!!
   :<!
   :<!!))

(in-package :cl-async-channel)

(defmacro >! ((data channel &optional result) &body body)
  (let ((x (or result (gensym))))
    `(cl-async-channel:send
      ,data
      ,channel
      (lambda (,x)
	,@ (if result () `((declare (ignorable ,x))))
	,@body))))

(defun >!! (data channel)
  (let ((res))
    (as:with-event-loop ()
      (>! (data channel tmp)
	(setf res tmp)))
    res))

(defmacro <! ((channel &optional result) &body body)
  (let ((x (or result (gensym))))
    `(cl-async-channel:recv
      ,channel
      (lambda (,x)
	,@ (if result () `((declare (ignorable ,x))))
	,@body))))

(defun <!! (channel)
  (let ((res))
    (as:with-event-loop ()
      (<! (channel tmp)
	(setf res tmp)))
    res))
