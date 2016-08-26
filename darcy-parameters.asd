;;;; darcy-parametes.asd

(asdf:defsystem #:darcy-parameters
  :description "Describe darcy-parameters here"
  :author "Alexey Cherkaev (mobius-eng)"
  :license "MIT"
  :depends-on (#:darcy-model
               #:parameters
               #:closer-mop
               #:parse-number)
  :serial t
  :components ((:file "package")
               (:file "unit-conversion")
               (:file "darcy-parameters")))

