(in-package #:darcy-parameters)

;; * CONSTRUCTOR: abbreviation function
(defun constructor (class-name)
  "Simple abbreviation: (CONSTRUCTOR CLASS-NAME)
returns a function (of a plist) applying MAKE-INSTANCE
to CLASS-NAME and plist-args"
  (lambda (&rest args)
    (apply #'make-instance class-name args)))


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
    (push (list (parameter-base-id model) model-maker)
          *unsaturated-model-makers*)))

;; Need to register pre-defined models
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
     :units "L/m2.h"
     :description "Inlet flow rate"
     :value-transformer (get-conversion "L/m2.h")))
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
     :units "L/m2.h"
     :description "Inlet flow rate"
     :value-transformer (get-conversion "L/m2.h"))
    (parameter
     :name "Frequency"
     :id :fluctuation-frequency
     :value 1d0
     :units "1/hour"
     :description "Frequency of fluctuation"
     :value-transformer (get-conversion "1/hour"))
    (parameter
     :name "Delay"
     :id :fluctuation-delay
     :value 0d0
     :units "hour"
     :description "Delay of fluctuation"
     :value-transformer (get-conversion "hour")))
   :constructor (constructor 'fluctuating-inlet-discharge)))

(define-units ("%" x) (/ x 100d0))

;; ** Default NOISY-INLET-DISCHARGE
(defun make-default-noisy-inlet-discharge ()
  "Produces default noisy inlet discharge model
with default 0% noise."
  (parameter
   :name "Noisy inlet specific discharge"
   :children
   (list
    (parameter
     :name "Noise"
     :id :inlet-discharge-noise
     :value 0d0
     :units "%"
     :description "Level of noise as the % of current flow rate"
     :value-transformer (get-conversion "%"))
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
    (push (list (parameter-base-id instance)
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
(defun uniform-darcy-model (&key
                              (conductivity (instantiate-object
                                             (make-default-conductivity)))
                              (unsaturated (instantiate-object
                                            (make-default-unsaturated-models)))
                              (height 1d0)
                              (mesh-points-number 20)
                              (inlet-discharge
                               (instantiate-object
                                (make-default-inlet-discharge-options))))
  "Creates uniform Darcy model. CONDUCTIVITY and UNSATURATED are provided
as singular objects and are broadcastes uniformly to each discretized volume"
  (let ((dz (coerce (/ height mesh-points-number) 'double-float)))
    (make-instance 'darcy
      :inlet-discharge inlet-discharge
      :unsaturated-models (fill-array unsaturated mesh-points-number)
      :conductivities (fill-array conductivity mesh-points-number)
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
     :name "Mesh points number"
     :id :mesh-points-number
     :value 20
     :units "-"
     :description "Number of spatial discretization points")
    (make-default-conductivity)
    (make-default-unsaturated-models)
    (make-default-inlet-discharge-options))
   :constructor (lambda (&rest args)
                  (apply #'uniform-darcy-model args))))

(defvar *name-substitutions*
  (list
   :rate :inlet-flow-rate
   :mesh-size :mesh-points-number
   :brooks-corey :brooks-corey-mualem
   :vg :van-genuchten
   :bcm :brooks-corey-mualem
   :brooks-corey :brooks-corey-mualem
   :k :intrinsic-permeability
   :nu :liquid-viscosity
   :lambda :pore-size-distribution-index
   :l :mualem-exponent
   :thetar :residual-water-content
   :thetas :saturated-water-content)
  "PList of names that can be used instead of proper parameter names
(in YAML files)")



;; * Simulation probem
;; ** Definition
(defclass darcy-simulation ()
  ((darcy
    :initarg :darcy
    :documentation
    "Darcy model instance to simulation")
   (final-time
    :initarg :final-time
    :documentation
    "Final time of simulation (in seconds)")
   (output-time-interval
    :initarg :output-time-interval
    :documentation
    "Interval with which produce the output")
   (plot-time-interval
    :initarg :plot-time-interval
    :documentation
    "Interval for plots: usualy longer than OUTPUT-TIME-INTERVAL")
   (initial-saturation
    :initarg :initial-saturation
    :documentation
    "Initial saturation for simulation")
   (simulation-result
    :initform nil
    :documentation
    "Storage for the result of simulation"))
  (:documentation
   "Full representation of the Darcy model simulation"))

;; ** Simulation
(defmethod simulate ((model darcy-simulation))
  (with-slots (darcy final-time output-time-interval
                     plot-time-interval initial-saturation
                     simulation-result)
      model
    (setf
     simulation-result
     (darcy-evolve darcy
                   (fill-array (coerce initial-saturation 'double-float)
                               (darcy-size darcy)
                               'double-float)
                   final-time
                   output-time-interval))))

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
     :units "hour"
     :value-transformer (get-conversion "hour")
     :description "Final time of simulation")
    (parameter
     :name "Output time interval"
     :id :output-time-interval
     :value 1d0
     :units "hour"
     :value-transformer (get-conversion "hour")
     :description "Provide outputs for every interval")
    (parameter
     :name "Plot time interval"
     :id :plot-time-interval
     :value 3d0
     :units "hour"
     :value-transformer (get-conversion "hour")
     :description "Time interval for each output series")
    (parameter
     :name "Initial saturation, s"
     :id :initial-saturation
     :value 0.01d0
     :units "-"
     :description "Initial effective saturation for simulation"))
   :constructor (constructor 'darcy-simulation)))

;; TODO: Simulate button - need to update the interface
;; TODO: Table view the result
;; TODO: Plot the results
