(defsystem :cl-async-channel
  :depends-on (:cl-async :bordeaux-threads)
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "cl-async-channel")
		 (:file "queue")
		 (:file "channel")))))
