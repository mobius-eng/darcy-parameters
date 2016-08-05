(in-package parameters)

;; * Parameters setting framework

;; ** Some utils 
(defun replace-characters (string parts replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (let ((replaced nil))
      (with-input-from-string (in (string-trim '(#\space) string))
        (loop for character = (read-char in nil nil)
           until (null character)
           if (find character parts :test test)
           do (unless replaced
                (write-char replacement out)
                (setf replaced t))
           else
           do (write-char character out)
           (setf replaced nil)
           end)))))

(defun make-keyword-id (string)
  (intern (string-upcase
           (replace-characters string '(#\space #\. #\, #\:) #\-))
          'keyword))

;; ** Common structure
(defclass parameter-base()
  ((parent
    :initarg :parent
    :initform nil
    :accessor parameter-base-parent
    :documentation "Parent parameter, responsible for setting the current one")
   (constructor
    :initarg :constructor
    :initform #'identity
    :reader parameter-base-constructor
    :documentation "Function that creates an instance corresponding to a parameter")
   (id
    :initarg :id
    :type :keyword
    :reader parameter-base-id
    :documentation "Parameter ID for quick access")
   (name
    :initarg :name
    :type string
    :reader parameter-base-name
    :documentation "Parameter name")
   (description
    :initarg :description
    :initform ""
    :type string
    :reader parameter-base-description
    :documentation "Description of the parameter"))
  (:documentation "Base class for all parameters"))


(defmethod initialize-instance :after ((object parameter-base) &key)
  (unless (slot-boundp object 'id)
    (with-slots (id name) object
      (setf id (make-keyword-id name)))))

(defgeneric instantiate-object (parameter))


;; ** Single value parameters
;; *** Common
;; (defclass single-value-parameter ()
;;   ((value
;;     :initarg :value
;;     :type t
;;     :accessor single-value-parameter-value
;;     :documentation "Parameter value")))

(defgeneric parameter-value (parameter))
(defgeneric (setf parameter-value) (newvalue parameter))

;; *** Simple parameter
;; =VALUE-TRANSFORMER= might not be necessary but it is convenient: it
;; provides an extra level of decopling
(defclass parameter (parameter-base)
  ((value
    :initarg :value
    :accessor parameter-value
    :type t
    :documentation "Value of the parameter")
   (units
    :initarg :units
    :initform ""
    :reader parameter-units
    :documentation "Parameter units of measure")
   (value-transformer
    :initarg :value-transformer
    :initform #'identity
    :reader parameter-value-transformer
    :documentation "Function that transforms the parameter value before setting it"))
  (:documentation "Parameter representation"))


(defmethod instantiate-object ((parameter parameter))
  (with-slots (constructor value value-transformer) parameter
    (funcall constructor (funcall value-transformer value))))

;; *** Perturbed parameter
(defclass perturbed-parameter (parameter)
  ((perturbation
    :initarg :perturbation
    :accessor perturbed-parameter-perturbation
    :documentation "Perturbation of the parameter")))

;; Introduce perturbation into VALUE-TRANSFORMER
(defmethod initialize-instance :after ((obj perturbed-parameter) &key)
  (with-slots (value-transformer) obj
    ;; save old value-transformer
    (let ((straight-transformer value-transformer))
      (setf value-transformer
            (lambda (x)
              ;; make sure to capture OBJ: need current PERTURBATION
              (with-slots (perturbation) obj
                (funcall
                 straight-transformer
                 (* x (+ 1d0 (random (* 2d0 perturbation)) (- perturbation))))))))))

;; *** Parameter-container-of-options
(defclass parameter-options (parameter-base)
  ((options
    :initarg :options
    :reader parameter-options-options
    :documentation "A vector of parameter options")
   (selection
    :initarg :selection
    :initform 0
    :accessor parameter-options-selection
    :documentation "Index of the current selection"))
  (:documentation "Selection of alternative parameters"))


(defmethod initialize-instance :after ((object parameter-options) &key)
  (with-slots (options) object
    (mapc (lambda (option)
            (setf (parameter-base-parent option) object))
          options)
    (setf options (apply #'vector options))))

(defmethod parameter-value ((parameter parameter-options))
  (with-slots (options selection) parameter
    (aref options selection)))

(defmethod instantiate-object ((parameter parameter-options))
  (instantiate-object (parameter-value parameter)))

;; Disable direct setting of PARAMETER-VALUE: this is a safe in this
;; configuration as =PARAMETER-BASE= does not have =VALUE= slot.
(defmethod (setf parameter-value) (newvalue (parameter parameter-options))
  (error "Cannot set value directly to PARAMETER-OPTIONS:\
          must select from the list of options"))

;; ** Parameter-container of other parameters
;; =CHILDREN= are the vector of =PARAMETER= (or it subclass =PARAMETER-OPTIONS=)
(defclass parameter-container (parameter-base)
  ((children
    :initarg :children
    :initform nil
    :reader parameter-container-children
    :documentation "Vector of individual parameters")
   (constructor :initform (lambda (&rest x) x)))
  (:documentation
   "Represents the group of parameters"))

(defmethod initialize-instance :after ((object parameter-container) &key)
  (with-slots (children constructor) object
    (mapc (lambda (child) (setf (parameter-base-parent child) object))
          children)))

;; Constructor for =PARAMETER-CONTAINER= is slightly different: it takes
;; the (as =&rest=) plist of the form =(id1 instance1 id2 instance2 ...)=, in contrast
;; to single-value parameters (where it is just a function of value only).
(defmethod instantiate-object ((parameter parameter-container))
  (with-slots (children constructor) parameter
    (let ((children-objects (mapcan
                             (lambda (p)
                               (list (parameter-base-id p)
                                     (instantiate-object p)))
                             children)))
      (apply constructor children-objects))))



;; ** Helpers: to reduce verbosity
(defun single-parameter (name value
                  &key (id (make-keyword-id name)) (description "") (units "")
                    (value-transformer #'identity) (constructor #'identity)
                    parent)
  (make-instance 'parameter
    :name name
    :id id
    :value value
    :units units
    :description description
    :value-transformer value-transformer
    :constructor constructor
    :parent parent))

(defun perturbed-parameter (name value
                            &key (id (make-keyword-id name)) (description "")
                              (units "") (value-transformer #'identity)
                              (constructor #'identity) (perturbation 0d0)
                              parent)
  (make-instance 'perturbed-parameter
    :name name
    :id id
    :value value
    :units units
    :description description
    :value-transformer value-transformer
    :constructor constructor
    :parent parent
    :perturbation perturbation))


(defun parameter-options (name options
                          &key (id (make-keyword-id name)) (description "")
                            (constructor #'identity) (selection 0)
                            parent)
  (make-instance 'parameter-options
    :name name
    :id id
    :options options
    :selection selection
    :description description
    :constructor constructor
    :parent parent))

(defun parameter-container (name children
                            &key (id (make-keyword-id name)) (description "")
                              (constructor (lambda (&rest x) x)) parent)
  (make-instance 'parameter-container
    :name name
    :id id
    :children children
    :constructor constructor
    :description description
    :parent parent))


(defun parameter (&rest args &key value perturbation options children &allow-other-keys)
  (cond ((and value perturbation)
         (apply #'make-instance 'perturbed-parameter args))
        (value (apply #'make-instance 'parameter args))
        (options (apply #'make-instance 'parameter-options args))
        (children (apply #'make-instance 'parameter-container args))
        (t (error "Incosistent parameter list: one of\
             VALUE PERTURBATION OPTIONS CHILDREN\
             are required"))))
