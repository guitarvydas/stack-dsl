(in-package :stack-dsl)


;; stack of typed values
;; 2 stacks for each type - "input" and "output"

(defun %type-check-failure (expected val)
  (error (format nil "~%exected type ~a, but got ~a~%" expected val)))

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

(defmethod %ensure-type ((self %typed-value) (obj T))
  (%type-check-failure "internal failure: object must be a %typed-value" obj))

(defmethod %ensure-type ((self %or-type) (obj %typed-value))
  (if (member (%type obj) (%type self))
      :ok
      (%type-check-failure (%type self) (%type obj))))

(defmethod %ensure-type ((self %typed-value) (obj %typed-value))
  (if (eq (%type obj) (%type self))
      :ok
      (%type-check-failure (%type self) (%type obj))))

(defmethod %ensure-type ((self %bag) (obj %bag))
  (if (eq (%element-type self) (%element-type obj))
      :ok
      (%type-check-failure self obj)))

(defmethod %ensure-type ((self %map) (obj %map))
  (if (eq (%element-type self) (%element-type obj))
      :ok
      (%type-check-failure self obj)))

(defmethod %ensure-type ((self %enum) (obj %typed-value))
  (if (eq 'enum (%type obj))
      (if (= (length (%value-list self)) (length (%value-list obj)))
	  (if (cl:every #'(lambda (x) (member x (%value-list self))) (%value-list obj))
	      :ok
	      (%type-check-failure "enum values must match" obj))
	  (%type-check-failure self obj))
      (%type-check-failure "must be an enum" obj))
  T)


;; only :bag and :map are appendable...
(defmethod %ensure-appendable ((self T))
  (%type-check-failure "appendable %typed-value (:bag or :map)" self))

(defmethod %ensure-appendable ((self %typed-value))
  (%type-check-failure "appendable (:bag or :map)" self))

(defmethod %ensure-appendable ((self %bag))
  t)

(defmethod %ensure-appendable ((self %map))
  t)


(defun test-stack-dsl ()
  #+nil(%ensure-type 5 6)
  (let ((x (make-instance '%typed-value)))
    #+nil(%ensure-type x 7)
    (let ((y (make-instance '%typed-value)))
      (%ensure-type x y)
      (setf (%type x) 'a)
      #+nil(%ensure-type x y)
      (setf (%type y) 'a)
      (%ensure-type x y)
      (let ((enum-a (make-instance '%enum))
	    (enum-b (make-instance '%enum)))
	(setf (%value-list enum-a) '(1 2 3))
	(setf (%value-list enum-b) '(1))
	#+nil(%ensure-type enum-a enum-b)
	#+nil(%ensure-type enum-b enum-a)
	(%ensure-type enum-a enum-a)
	(let ((enum-c (make-instance '%enum)))
	  (setf (%value-list enum-c) '(2 3 1))
	  (%ensure-type enum-a enum-c)
	  (%ensure-type enum-c enum-a)
	  )
	(let ((bag-a (make-instance '%bag)))
	  #+nil(%ensure-type bag-a x)
	  (let ((bag-b (make-instance '%bag)))
	    #+nil(%ensure-type bag-a bag-b)
	    (setf (%element-type bag-a) 'a)
	    #+nil(%ensure-type bag-a bag-b)
	    (setf (%element-type bag-b) 'z)
	    #+nil(%ensure-type bag-a bag-b)
	    (%ensure-type bag-a bag-a)
	    (setf (%element-type bag-b) 'a)
	    (%ensure-type bag-a bag-a)
	    ))
	(let ((map-a (make-instance '%bag)))
	  (let ((map-b (make-instance '%bag)))
	    (setf (%element-type map-a) 'a)
	    (setf (%element-type map-b) 'z)
	    (%ensure-type map-a map-a)
	    (setf (%element-type map-b) 'a)
	    (%ensure-type map-a map-a)
	    ))
	  )))
  (let ((bag-a (make-instance '%bag)))
    #+nil(%ensure-appendable 8)
    (let ((x (make-instance '%typed-value)))
      #+nil(%ensure-appendable x)
      (%ensure-appendable bag-a)))
  "test finished")

(defun assert-is-stack (stack)
  (assert (subtypep (type-of stack) '%typed-stack)))

(defun %push-empty (stack)
  ;; push some sort of empty indicator onto the stack
  (assert-is-stack stack)
  (let ((eltype (%element-type stack)))
    (let ((obj (make-instance eltype)))
      (push obj (%stack stack)))))
