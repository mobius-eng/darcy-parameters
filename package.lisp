;;;; package.lisp

(defpackage :darcy-parameters
  (:use #:cl #:parameters #:darcy-use)
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
           #:uniform-darcy-model
           #:constructor
           #:*name-substitutions*))
