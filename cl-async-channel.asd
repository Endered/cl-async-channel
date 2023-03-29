(defsystem :cl-async-channel
  :depends-on (:cl-async :bordeaux-threads)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("cl-async-channel"))
		 (:file "cl-async-channel" :depends-on ("channel"))
		 (:file "queue")
		 (:file "channel" :depends-on ("queue" "util"))
		 (:file "util")))))
