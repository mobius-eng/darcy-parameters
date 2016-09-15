(in-package darcy-parameters)

;; * Utils
;; ** CONSTRUCTOR: abbreviation function
(defun constructor (class-name)
  "Simple abbreviation: (CONSTRUCTOR CLASS-NAME)
returns a function (of a plist) applying MAKE-INSTANCE
to CLASS-NAME and plist-args"
  (lambda (&rest args)
    (apply #'make-instance class-name args)))

;; ** Extend PARAMETER to convert units automatically
(define-keyword-method parameter (:value :units-with-conversion)
    (&rest args &key units-with-conversion &allow-other-keys)
  (let ((arg-list (list*
                   :units units-with-conversion
                   :constructor (get-conversion units-with-conversion)
                   (loop for (key value) on args by #'cddr
                      when (not (eq key :units-with-conversion))
                      append (list key value)
                      end))))
    (apply #'parameter arg-list)))

;; * Default CONDUCTIVITY
(defun make-default-conductivity ()
  "Make default conductivity for a water at 20C and
typical (by order of magnitude) intrinsic permeability"
  (parameter
   :name "Conductivity"
   :id :conductivity
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
   :constructor (constructor 'conductivity)))

(defun make-perturbed-conductivity ()
  (let ((p (perturb-parameter! (make-default-conductivity) 0d0)))
    (setf (parameter-name p) "Perturbed Conductivity")
    (setf (parameter-id p) :perturbed-conductivity)
    p))

;; * Default unsaturated models
;; ** Common parameters
;; General common parameters
(defun make-default-unsaturated-common ()
  "Produces the list of parameters common for unsaturated model"
  (list
   (parameter
    :name "θ(saturated)"
    :id :saturated-water-content
    :value 0.3d0
    :units "-"
    :description "Saturated water content: max(V[liquid]/V[total])")
   (parameter
    :name "θ(residual)"
    :id :residual-water-content
    :value 0.01d0
    :units "-"
    :description "Residual water content: min(V[liquid]/V[total])")
   (parameter
    :name "ψ(bubbling)"
    :id :bubbling-pressure
    :value 0.33333d0
    :units "m"
    :description "Min of abs capillary pressure at which gaseous phase moves")))

;; Mualem common parameter
(defun make-default-mualem ()
  "A common parameter for Mualem-type models"
  (parameter
   :name "L"
   :id :mualem-exponent
   :value 0.5d0
   :units "-"
   :description "Mualem exponent"))

;; ** Default VAN-GENUCHTEN
(defun make-default-van-genuchten ()
  "Produces default van Genuchten unsaturated model with
typical values of bubbling pressure and N"
  (parameter
   :name "Van Genuchten"
   :id :van-genuchten
   :children
   (list*
    (parameter
     :name "n"
     :id :n
     :value 1.5d0
     :units "-"
     :description "Van Genuchten exponent")
    (make-default-mualem)
    (make-default-unsaturated-common))
   :constructor (constructor 'van-genuchten)))

(defun make-perturbed-van-genuchten ()
  (let ((p (perturb-parameter! (make-default-van-genuchten)
                               (list :n 0.1d0))))
    (setf (parameter-name p) "Perturbed Van Genuchten")
    (setf (parameter-id p) :perturbed-van-genuchten)
    p))

;; ** Default BROOKS-COREY
(defun make-default-brooks-corey-mualem ()
  "Produces default Brooks-Corey-Mualem unsaturated model
with typical values of bubbling pressure and pore size
distribution index (lambda)"
  (parameter
   :name "Brooks-Corey-Mualem"
   :id :brooks-corey-mualem
   :children
   (list*
    (parameter
     :name "λ"
     :id :pore-size-distribution-index
     :value 2d0
     :units "-"
     :description "Pore size distribution index")
    (make-default-mualem)
    (make-default-unsaturated-common))
   :constructor (constructor 'brooks-corey-mualem)))

(defun make-perturbed-brooks-corey-mualem ()
  (let ((p (perturb-parameter! (make-default-brooks-corey-mualem)
                               (list :pore-size-distribution-index 0.1d0))))
    (setf (parameter-name p) "Perturbed Brooks-Corey-Mualem")
    (setf (parameter-id p) :perturbed-brooks-corey-mualem)
    p))

;; ** Default UNSATURATED-MODELS
(defvar *unsaturated-model-makers* nil
  "Alist of available unsaturated model makers. Each entry:
    (MODEL-ID MAKER-FUNCTION)
where:
    MODEL-ID is a keyword identifying a model
    MAKER-FUNCTION is a function of no arguments that
                   produces default PARAMETER-object for the model")

(defun register-new-unsaturated-model (model-maker)
  "Register a new unsaturated model by providing
a function of no argument that makes it.
MODEL-ID is taken from the produced model"
  (let ((model (funcall model-maker)))
    (push (list (parameter-id model) model-maker)
          *unsaturated-model-makers*)))

;; Need to register pre-defined models
(register-new-unsaturated-model #'make-perturbed-brooks-corey-mualem)
(register-new-unsaturated-model #'make-perturbed-van-genuchten)
(register-new-unsaturated-model #'make-default-brooks-corey-mualem)
(register-new-unsaturated-model #'make-default-van-genuchten)

(defun make-default-unsaturated-models ()
  "Make default set of unsaturated model options"
  (parameter
   :name "Unsaturated models"
   :id :unsaturated
   :options
   (loop for (model-id model-maker) in *unsaturated-model-makers*
      collect (funcall model-maker))))

;; * Default inlet discharge
;; ** Default CONSTANT-INLET-DISCHARGE
(defun make-default-constant-inlet-discharge ()
  "Produces default constant inlet discharge model
with flow rate of 10 L/m2 h. Provides the conversion to SI units"
  (parameter
   :name "Constant inlet discharge"
   :children
   (list
    (parameter
     :name "Flow rate"
     :id :inlet-flow-rate
     :value 10d0
     :units-with-conversion "L/m2.h"
     :description "Inlet flow rate"))
   :constructor (constructor 'constant-inlet-discharge)))

(define-units ("1/h" x) (/ x 3600d0))
(define-units ("1/hour" x nil) (/ x 3600d0))

;; ** Default FLUCTUATING-INLET-DISCHARGE
(defun make-default-fluctuating-inlet-discharge ()
  "Produces default fluctuating inlet discharge model
with max rate of 10 L/m2 h, 0 delay, and 1 1/h frequency.
Provides all the necessary transformers to SI units"
  (parameter
   :name "Fluctuating inlet discharge"
   :children
   (list
    (parameter
     :name "Flow rate"
     :id :inlet-flow-rate
     :value 10d0
     :units-with-conversion "L/m2.h"
     :description "Inlet flow rate")
    (parameter
     :name "Frequency"
     :id :fluctuation-frequency
     :value 1d0
     :units-with-conversion "1/hour"
     :description "Frequency of fluctuation")
    (parameter
     :name "Delay"
     :id :fluctuation-delay
     :value 0d0
     :units-with-conversion "hour"
     :description "Delay of fluctuation"))
   :constructor (constructor 'fluctuating-inlet-discharge)))

(define-units ("%" x) (/ x 100d0))

;; ** Default NOISY-INLET-DISCHARGE
(defun make-default-noisy-inlet-discharge ()
  "Produces default noisy inlet discharge model
with default 0% noise."
  (parameter
   :name "Noisy inlet discharge"
   :children
   (list
    (parameter
     :name "Noise"
     :id :inlet-discharge-noise
     :value 0d0
     :units-with-conversion "%"
     :description "Level of noise as the % of current flow rate")
    (parameter
     :name "Base flow rate"
     :id :base-inlet-discharge
     :options
     (list (make-default-constant-inlet-discharge)
           (make-default-fluctuating-inlet-discharge))))
   :constructor (constructor 'noisy-inlet-discharge)))

;; Default options for inlet discharge
(defvar *inlet-discharge-model-makers* nil
  "Alist of possible inlet discharge model makers. Each entry:
    (MODEL-ID MAKER-FUNCTION)
where:
    MODEL-ID is a keyword identifying a model
    MAKER-FUNCTION is a function of no arguments that
                   produces default PARAMETER-object for the model")

(defun register-new-inlet-discharge (maker)
  (let ((instance (funcall maker)))
    (push (list (parameter-id instance)
                maker)
          *inlet-discharge-model-makers*)))

(register-new-inlet-discharge #'make-default-fluctuating-inlet-discharge)
(register-new-inlet-discharge #'make-default-noisy-inlet-discharge)
(register-new-inlet-discharge #'make-default-constant-inlet-discharge)

(defun make-default-inlet-discharge-options ()
  (parameter
   :name "Inlet discharge"
   :id :inlet-discharge
   :options
   (loop for (name maker) in *inlet-discharge-model-makers*
      collect (funcall maker))))

;; * Darcy model
(defun uniform-darcy-model (&key conductivities-unsaturated-models height inlet-discharge)
  "Creates uniform Darcy model. CONDUCTIVITY and UNSATURATED are provided
as singular objects and are broadcastes uniformly to each discretized volume"
  (let* ((unsaturated-models (apply #'vector (getf conductivities-unsaturated-models :unsaturated)))
         (conductivities (apply #'vector (getf conductivities-unsaturated-models :conductivity)))
         (dz (coerce (/ height (length conductivities)) 'double-float)))
    (make-instance 'darcy
      :inlet-discharge inlet-discharge
      :unsaturated-models unsaturated-models
      :conductivities conductivities
      :space-step dz)))

(defun make-default-darcy-model ()
  "Produces default parameter for full Darcy model"
  (parameter
   :name "Darcy model"
   :id :darcy
   :children
   (list
    (parameter
     :name "Column height"
     :id :height
     :value 1d0
     :units "m"
     :description "Height of the packing/column")
    (parameter
     :name "Conductivities and unsaturated models"
     :id :conductivities-unsaturated-models
     :number-of-instances 20
     :children (list (make-default-conductivity)
                     (make-default-unsaturated-models)))
    (make-default-inlet-discharge-options))
   :constructor (lambda (&rest args)
                  (apply #'uniform-darcy-model args))))

;; (parameter
;;      :name "Mesh points number"
;;      :id :mesh-points-number
;;      :value 20
;;      :units "-"
;;      :description "Number of spatial discretization points")
;;     (make-default-conductivity)
;;     (make-default-unsaturated-models)


(defvar *name-substitutions*
  (list
   ;; General
   :mesh-size :mesh-points-number
   ;; Conductivity
   :k :intrinsic-permeability
   :nu :liquid-viscosity
   ;; Unsaturated
   :brooks-corey :brooks-corey-mualem
   :vg :van-genuchten
   :vgm :van-genuchten
   :bcm :brooks-corey-mualem
   :lambda :pore-size-distribution-index
   :l :mualem-exponent
   :thetar :residual-water-content
   :thetas :saturated-water-content
   :psib :bubbling-pressure
   :psi-b :bubbling-pressure
   ;; Inlet
   :inlet :inlet-discharge
   :constant :constant-inlet-discharge
   :fluctuating :fluctuating-inlet-discharge
   :noisy :noisy-inlet-discharge
   :rate :inlet-flow-rate
   ;; Simulation
   :s0 :initial-saturation
   )
  "PList of names that can be used instead of proper parameter names
  (in YAML files)")


;; ** Parameters
(defun make-default-darcy-simulation ()
  (parameter
   :name "Darcy Model Simulation"
   :children
   (list
    (make-default-darcy-model)
    (parameter
     :name "Final time"
     :id :final-time
     :value 24d0
     :units-with-conversion "hour"
     :description "Final time of simulation")
    (parameter
     :name "Output time interval"
     :id :output-time-interval
     :value 1d0
     :units-with-conversion "hour"
     :description "Provide outputs for every interval")
    (parameter
     :name "Fine time interval"
     :id :fine-time-interval
     :value 0.1d0
     :units-with-conversion "hour"
     :description "Fine time interval for calculating inlet and outlet")
    (parameter
     :name "Plot time interval"
     :id :plot-time-interval
     :value 3d0
     :units-with-conversion "hour"
     :description "Time interval for each output series")
    (parameter
     :name "Initial saturation, s"
     :id :initial-saturation
     :value 0.01d0
     :units "-"
     :description "Initial effective saturation for simulation"))
   :constructor (constructor 'darcy-simulation)))

