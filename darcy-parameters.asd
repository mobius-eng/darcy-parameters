;;;; darcy-parametes.asd

(asdf:defsystem #:darcy-parameters
  :description "Describe darcy-parameters here"
  :author "Alexey Cherkaev (mobius-eng)"
  :license "MIT"
  :depends-on (#:darcy-model
               #:parameters
               #:parameters-extra
               #:closer-mop
               #:parse-number
               #:cl-slice)
  :serial t
  :components ((:file "package")
               (:file "unit-conversion")
               (:file "darcy-parameters")
               (:file "darcy-interface")))

