(in-package #:darcy-parametes)


(defvar *conductivity*
  (parameter
   :name "Conductivity"
   :children
   (list (parameter
          :name "Viscosity (ν)"
          :id :liquid-viscosity
          :value 1d-6
          :units "m2/s"
          :description "Liquid kinematic viscosity")
         (parameter
          :name "Intrinsic permeability, k"
          :id :intrinsic-permeability
          :value 1d-9
          :units "m2"
          :description "Medium intrinsic permeability"))
   :constructor (lambda (&rest args)
                  (apply #'make-instance 'conductivity args))))

(defvar *unsaturated-base-parameter-list*
  )


(defvar *unsaturated-common*
  '((:name "Saturated water content"
     :value 0.3d0
     :untis "-"
     :description "max(V[liquid]/V[total])"
     :keyword :saturated-water-content)
    (:name "Residual water content"
     :value 0.05d0
     :untis "-"
     :description "min(V[liquid]/V[total])"
     :keyword :residual-water-content)
    (:name "Bubbling pressure"
     :value 0.3d0
     :units "m"
     :description "Min of abs capillary pressure at which gaseous phase moves")))

(defvar *mualem*
  '((:name "Mualem exponent"
     :value 0.5d0
     :units "-"
     :description "Mualem theory exponent"
     :keyword :mualem-exponent)))

(defvar *unsaturated-models*
  `((van-genuchten
     (,@*unsaturated-common*
      ,@*mualem*
      (:name
       "n"
       :value 1.5d0
       :units "-"
       :description "van Genuchten fitting parameter (affects m = 1 - 1 / n)"
       :keyword :n)))
    (brooks-corey-mualem
     (,@*unsaturated-common*
      ,@*mualem*
      (:name
       "lambda"
       :value 2d0
       :units "-"
       :description "Pore size distribution index"
       :keyword :pore-size-distribution-index)))))


(defvar *inlet-flow-rate*
  `(:name
    "Inlet flow rate"
    :value ,(/ 10d0 1000 3600)
    :units "m/s"
    :description ""
    :keyword :inlet-flow-rate))

(defvar *inlet-specific-dischages*
  `((constant-inlet-discharge
     (,*inlet-flow-rate*))
    (fluctuating-inlet-discharge
     (,*inlet-flow-rate*
      (:name
       "Frequency"
       :value ,(/ 3600d0)
       :units "1/s"
       :description ""
       :keyword :fluctuation-frequency)
      (:name
       "Delay"
       :value 0d0
       :units "s"
       :description ""
       :keyword :fluctuation-delay)))))

(defvar *noisy-inlet-specific-discharge*
  `((noisy-inlet-discharge
     (:name
      "Noise"
      :value 0.05d0
      :units "-"
      :description "Noise on inlet flow rate value"
      :keyword :inlet-discharge-noise)
     (:name
      "Base flow rate"
      :value ,*inlet-specific-dischages*
      :units "-"
      :description "Underlying inlet specific discharge"
      :keyword :base-inlet-discharge))))
