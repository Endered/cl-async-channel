(defsystem :cl-async-channel
  :depends-on (:cl-async :bordeaux-threads)
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "queue")))))
