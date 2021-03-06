;;;; package.lisp

(defpackage :unit-conversion
  (:use #:cl)
  (:export #:register-units-converter
           #:deregister-units-converter
           #:get-conversion
           #:has-conversion-p
           #:convert-units
           #:define-units))


(defpackage :darcy-parameters
  (:use #:cl #:parameters #:keyword-dispatch #:darcy-use #:unit-conversion)
  (:export #:make-default-conductivity
           #:make-default-van-genuchten
           #:make-default-brooks-corey-mualem
           #:make-default-unsaturated-models
           #:make-default-constant-inlet-discharge
           #:make-default-noisy-inlet-discharge
           #:make-defualt-fluctuating-inlet-discharge
           #:make-default-inlet-discharge-options
           #:*unsaturated-model-makers*
           #:*inlet-discharge-model-makers*
           #:register-new-inlet-discharge
           #:register-new-unsaturated-model
           #:make-default-darcy-model
           #:make-default-darcy-simulation
           #:uniform-darcy-model
           #:constructor
           #:*name-substitutions*))


(defpackage :darcy-interface
  (:use #:cl+qt #:parameters #:parameters-interface #:parameters-yaml
        #:darcy-parameters #:darcy-use #:cl-slice))
