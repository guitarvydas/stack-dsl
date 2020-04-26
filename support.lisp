(in-package :stack-dsl)


;; stack of typed values
;; 2 stacks for each type - "input" and "output"

;; all items must be %typed-value or %typed-struct or %typed-stack or %bag or %map or %string
(defclass %typed-value ()
  ((%type :accessor %type :initform :no-type :initarg :%type)
   (val   :accessor val   :initform :no-value)))

(defclass %or-type (%typed-value)
  ((%type-list :accessor %type-list :initform nil :initarg :type-list)))
  
(defclass %typed-stack ()
  ((%element-type :accessor %element-type :initform :no-type :initarg :type)
   (%stack :accessor %stack :initform nil)))

(defclass %bag (%typed-value)
  ((%element-type :accessor %element-type :initform :no-type :initarg :type)
   (lis :accessor lis :initform nil))
  (:default-initargs
   :%type '%bag))

(defclass %map (%typed-value)
  ((%element-type :accessor %element-type :initform :no-type :initarg :type)
   (lis :accessor lis :initform nil))
  (:default-initargs
   :%type '%map))

(defclass %string (%typed-value)
  ()
  (:default-initargs 
   :%type 'string))

(defclass %enum (%typed-value)
  ((%value-list :accessor %value-list))
  (:default-initargs
   :%type 'enum))



;; applicable to :bag or :map
;; for :bag, we don't care about order of items
;; for :map, items must be indexable
(defgeneric %append (self val))

(defun %ensure-existence (class-symbol)
  (let ((c (find-class class-symbol)))
    (when (null c)
      (error "class ~a has not been defined" class-symbol)))
  T)


;;;;;;;;;;;; type checking

(defmethod %ensure-type ((self T) obj)
  (%type-check-failure "internal failure: self must be a %typed-value" obj))

(defmethod %ensure-type ((self %type-value) (obj T))
  (%type-check-failure "internal failure: object must be a %typed-value" obj))

(defmethod %ensure-type ((self %or-type) (obj %typed-value))
  (member (%type obj) (%type self)))

(defmethod %ensure-type ((self %typed-value) (obj %typed-value)
  (eq (%type obj) (%type self)))

(defmethod %ensure-type ((self %enum) (obj %typed-value))
  (if (eq 'enum (%type obj))
      (if (cl:every #'(lambda (x) (member x (%value-list self))) (%value-list obj))
	  :ok
	  (%type-check-failure "enum values must match" obj))
      (%type-check-failure "must be an enum" obj))
  T)


;; only :bag and :map are appendable...
(defmethod %ensure-appendable ((self T))
  (declare (ignore self))
  (%type-check-failure "appendable %typed-value (:bag or :map)"))

(defmethod %ensure-appendable ((self %typed-value))
  (declare (ignore self))
  (%type-check-failure "appendable (:bag or :map)"))

(defmethod %ensure-appendable ((self %bag))
  t)

(defmethod %ensure-appendable ((self %map))
  t)


;;; operations

;; none of these low-level operations check the type -
;; checking must be done by the caller

(defun %output (input-stack output-stack)
  ;; "return" the top item on the input-stack by pushing it onto the output
  ;; stack
  ;; no need to type-check, since DSL guarantees that the input and output types
  ;; are the same
  (assert (subtypep (type-of input-stack) '%typed-stack))
  (assert (subtypep (type-of output-stack) '%typed-stack))
  (let ((v (first (%stack input-stack))))
    (push v (%stack output-stack))))

(defun %push-empty (stack)
  ;; push some sort of empty indicator onto the stack
  (assert (subtypep (type-of stack) '%typed-stack))
  (let ((eltype (%element-type stack)))
    (let ((obj (make-instance eltype)))
      (setf (val obj) :unbound)
      (push obj (%stack stack)))))

(defun %replace-top (stack v)
  ;; assign other to top of stack (no push)
  (assert (subtypep (type-of stack) '%typed-stack))
  (pop (%stack stack))  ;; pop-push is replacement
  (push v (%stack stack)))

(defun %pop (stack)
  (assert (subtypep (type-of stack) '%typed-stack))
  (pop (%stack stack)))

(defun %top (stack)
  (assert (subtypep (type-of stack) '%typed-stack))
  (first (%stack stack)))

(defun %type-check-failure (expected val)
  (error (format nil "~%exected type ~a, but got ~a~%" expected val)))

(defun %get-field (obj field-name)
  ;; return obj.field
  (slot-value field-name obj))

(defun %set-field (obj field-name val)
  ;; obj.field := val
  (setf (slot-value field-name obj) val))

(defmethod %append ((self %bag) (new-val %typed-value))
  (setf (lis self) (append (lis self) (list new-val))))

(defmethod %append ((self %map) (new-val %typed-value))
  (setf (lis self) (append (lis self) (list new-val))))

