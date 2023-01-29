(defsystem :cl-async-channel
  :depends-on (:cl-async)
  :components ((:module "src"
                :components
                ((:file "main")))))
