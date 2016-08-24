(in-package unit-conversion)

(defvar *skip-characters*
  (list #\space #\( #\) #\^))

(defvar *replace-characters*
  (list (cons #\/ "-PER-")
        (cons #\. "-")))

(defun produce-keyword (string)
  "Produce keyword out of the string by removing `illigal'
characters (e.g, `,', `.' ` ', etc.)"
  (if (symbolp string)
      string
      (let ((string (string-upcase string)))
        (intern
         (with-output-to-string (str)
           (loop for char across string
              unless (member char *skip-characters* :test #'eql)
              do (let ((replacement (assoc char *replace-characters*)))
                   (if replacement
                       (princ (cdr replacement) str)
                       (princ char str)))
              end))
         'keyword))))

(defvar *conversions*
  (make-hash-table :test 'eq)
  "Table of conversions from an arbitrary units to SI.
The key is interned keyword of the simplified units designator.
The entry is the function that converts this units to SI")

(defun register-units-converter (source-units conversion-function
                                 &optional (error-on-collision t))
  (let ((key (produce-keyword source-units)))
    (when (and error-on-collision (gethash key *conversions*))
      (error "The specified units ~A (~A) conversion is already defined.
Remove it first before replacing (see DEREGISTER-UNITS-CONVERTER)"
             source-units key))
    (setf (gethash key *conversions*) conversion-function)))

(defun deregister-units-converter (source-units)
  (let ((key (produce-keyword source-units)))
    (remhash key *conversions*)))

(defun get-conversion (units)
  "Returns the conversion function for UNITS to SI units.
By default, returns IDENTITY if conversion is not found.
Secondary value indicates if the conversion was found"
  (let ((key (produce-keyword units)))
    (gethash key *conversions* #'identity)))

(defun has-conversion-p (units)
  (let ((key (produce-keyword units)))
    (multiple-value-bind (result found-p) (gethash key *conversions*)
      (declare (ignore result))
      found-p)))

(defun convert-units (value units)
  "Converts VALUE of UNITS of measure to SI.
Secondary value indicates if the conversion was found"
  (multiple-value-bind (converter found) (get-conversion units)
    (values (funcall converter value) found)))

(defmacro define-units ((units-string var &optional (error-on-collision t))
                        &body conversion)
  `(register-units-converter
    ,units-string
    (lambda (,var)
      ,@conversion)
    ,error-on-collision))

;; Time
(define-units ("min" x nil) (* x 60d0))
(define-units ("h" x nil) (* x 3600d0))
(define-units ("hr" x nil) (* x 3600d0))
(define-units ("hrs" x nil) (* x 3600d0))
(define-units ("hour" x nil) (* x 3600d0))
(define-units ("hours" x nil) (* x 3600d0))
(define-units ("day" x nil) (* x 3600d0 24d0))
(define-units ("days" x nil) (* x 3600d0 24d0))
(define-units ("d" x nil) (* x 3600d0 24d0))
(define-units ("s" x nil) x)

;; Mass
(define-units ("mg" x nil) (/ x 1d6))
(define-units ("g" x nil) (/ x 1000d0))
(define-units ("kg" x nil) x)
(define-units ("t" x nil) (* x 1d3))
(define-units ("tonne" x nil) (* x 1d3))
(define-units ("lbs" x nil) (/ x  2.2046226218))

;; Length
(define-units ("um" x nil) (/ x 1d6))
(define-units ("mm" x nil) (/ x 1d3))
(define-units ("cm" x nil) (/ x 1d2))
(define-units ("dm" x nil) (/ x 10d0))
(define-units ("m" x nil) x)
(define-units ("km" x nil) (* x 1d3))

;; Area
(define-units ("cm2" x nil) (/ x 1d4))
(define-units ("km2" x nil) (* x 1d6))
(define-units ("m2" x nil) x)

;; Volume
(define-units ("cm3" x nil) (/ x 1d6))
(define-units ("ml" x nil) (/ x 1d6))
(define-units ("l" x nil) (/ x 1d3))
(define-units ("m3" x nil) x)

;; Molarity
(define-units ("mol" x nil) x)
(define-units ("kmol" x nil) (* x 1d3))
(define-units ("mmol" x nil) (/ x 1d3))

;; Density / concentration
(define-units ("g/ml" x nil) (* x 1d3))
(define-units ("g/cm3" x nil) (* x 1d3))
(define-units ("kg/m3" x nil) x)
(define-units ("mol/L" x nil) (* x 1d3))
(define-units ("kmol/m3" x nil) (* x 1d3))
(define-units ("mol/m3" x nil) x)

;; Velocity
(define-units ("km/h" x nil) (/ x 3600d0))
(define-units ("km/hr" x nil) (/ x 3600d0))
(define-units ("L/m^2.h" x nil) (/ x 1000d0 3600d0))

;; Diffusivity/Viscosity
(define-units ("m2/s" x nil) x)
(define-units ("cm2/s" x nil) (/ x 1d4))
