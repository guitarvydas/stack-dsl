(in-package :stack-dsl)


;; stack of typed values
;; 2 stacks for each type - "input" and "output"

;; all items must be %typed-value or %typed-struct or %typed-stack or %bag or %map or %string
(defclass %typed-value ()
  (%type :accessor %type :initform :no-type :initarg :type)
  (val   :accessor val   :initform :no-value))

(defclass %typed-struct ()
  ()) ;; every field must be of type %typed-value

(defclass %typed-stack ()
  (%element-type :accessor %element-type :initform :no-type :initarg :type)
  (%stack :accessor %stack :initform nil))

(defclass %bag ()
  (%element-type :accessor %element-type :initform :no-type :initarg :type)
  (lis :accessor lis :initform nil))

(defclass %map ()
  (%element-type :accessor %element-type :initform :no-type :initarg :type)
  (lis :accessor lis :initform nil))

(defclass %string (%typed-value)
  (:default-initargs :type 'string))

;; applicable to :bag or :map
;; for :bag, we don't care about order of items
;; for :map, items must be indexable
(defgeneric %append (self val))

(defmethod %append ((self %bag) (new-val %typed-value))
  (%ensure-type new-val (%type self))
  (setf (lis self) (append (lis self) (list new-val))))

(defmethod %append ((self %map) (new-val %typed-value))
  (%ensure-type new-val (%type self))
  (setf (lis self) (append (lis self) (list new-val))))
