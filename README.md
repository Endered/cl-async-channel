# cl-async-channel 
This suply go like channel system, work on asynchronous.
For work on asynchronous, this library uses [libuv](https://github.com/libuv/libuv) by [cl-async](https://github.com/orthecreedence/cl-async). So, please install shared object of libuv before use this.


## HOW TO USE 
Please use below functions without `make-channel` in cl-async's event loop

### make channel
```lisp
(make-channel {integer of channel bound})
```

### send to channel 
```lisp
(send {data} {channel} {callback function})
;; or 
(>! ({data} {channel} [{result of send}]) ...)
;; and callback must be like 
(lambda (x) ...)
;; currently the value of variable x is t
```

### recv from channel 
```lisp
(recv {channel} {callback function})
;; or
(<! ({channel} [{result of recv}]) ...)
;; and callback must be like 
(lambda (x) ...)
;; the value of variable x is data sended from channel
```

### select 
```lisp 
(select ((recv c1 x)
         (format t "recieved ~a from channel c1~%" x))
        ((send "HELLO" c2 x)
         (format t "sended "HELLO" to channel c2")))
```


### example
```lisp
(as:with-event-loop ()
  (let ((c1 (make-channel 0))
        (c2 (make-channel 0)))
    (labels ((producer (c n)
               (as:with-delay (1)
                 (when (< 0 n)
                   (send n c (lambda (tmp)
                               (declare (ignorable tmp))
                               (format t "sended ~a~%" n)
                               (producer c (1- n)))))))
             (consumer ()
               (select ((recv c1 x)
                        (format t "recieved ~a from c1~%" x)
                        (consumer))
                       ((recv c2 x)
                        (format t "recieved ~a from c2~%" x)
                        (consumer)))))
      (producer c1 10)
      (producer c2 10)
      (consumer))))
```

### <! and >!
`<!` and `>!` is simple macro of `recv` and `send`.
```lisp
(>! (x c y)
  (print y))
;; is convert to
(send x c (lambda (y)
            (print y)))

(<! (c x)
  (print x))
;; is convert to
(recv c (lambda (x)
          (print x)))

;; If you not need a result of sending/recieving, you can omit last argument.
(>! (x c)
  (format t "Sended ~a!!!~%" x))

(<! (c)
  (format t "Recieved some data!!!~%"))
```

Recommend to use these utility with `nest` macro.

```lisp
;; example
(as:with-event-loop ()
  (let ((c (make-channel 0)))
    (labels ((producer (n)
               (uiop:nest
                (when (< 0 n))
                (as:with-delay (1))
                (>! (n c))
                (producer (1- n))))
             (consumer ()
               (uiop:nest
                (<! (c x))
                (progn (format t "recieved ~a~%" x))
                (consumer))))
      (producer 10)
      (consumer))))
```
