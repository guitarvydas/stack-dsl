(in-package :stack-dsl)


;; stack of typed values
;; 2 stacks for each type - "input" and "output"

(defun %type-check-failure (val expected)
  (error (format nil "~%expected type ~a, but got ~a~%" expected val)))

;; all items must be %typed-value or %typed-struct or %typed-stack or %bag or %map or %string
(defclass %typed-value ()
  ((%type :accessor %type :initform :no-type :initarg :%type)
   (%value   :accessor %value   :initform :no-value)))

(defclass %or-type (%typed-value)
  ((%type-list :accessor %type-list :initform nil :initarg :type-list)))
  
(defclass %typed-stack ()
  ((%element-type :accessor %element-type :initform :no-type :initarg :element-type)
   (%stack :accessor %stack :initform nil)))

(defclass %bag (%typed-value)
  ((%element-type :accessor %element-type :initform :no-type :initarg :element-type)
   (lis :accessor lis :initform nil))
  (:default-initargs
   :%type '%bag))

(defclass %map (%typed-value)
  ((%map-element-type :accessor %map-element-type :initform :no-type :initarg :element-type)
   (ordered-list :accessor ordered-list :initform nil))
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

#+nil(defmethod %ensure-type ((self T) obj)
  (%type-check-failure "internal failure 1: self must be a %typed-value" obj))

#+nil(defmethod %ensure-type ((self %typed-value) (obj T))
  (%type-check-failure "internal failure 2: object must be a %typed-value" obj))

(defmethod %ensure-type ((self %typed-value) tysym)
  (if (eq tysym (%type self))
      :ok
      (%type-check-failure (%type self) tysym)))

(defmethod %ensure-type ((self %typed-value) (obj %typed-value))
  (if (eq (%type obj) (%type self))
      :ok
      (%type-check-failure (%type self) (%type obj))))

(defmethod %ensure-type ((self %or-type) (obj %typed-value))
  (if (member (%type obj) (%type-list self))
      :ok
      (%type-check-failure self obj)))

(defmethod %ensure-type ((self %bag) (obj %bag))
  (if (eq (%element-type self) (%type obj))
      :ok
      (%type-check-failure self obj)))

(defmethod %ensure-type ((self %map) (obj %map))
  (if (eq (%map-element-type self) (%type obj))
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


;;;; test 0 ;;;;

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
    (let (#+nil(x (make-instance '%typed-value)))
      #+nil(%ensure-appendable x)
      (%ensure-appendable bag-a)))
  (let ((or-a (make-instance '%or-type)))
    #+nil(%ensure-type or-a 5)
    (let ((a-var (make-instance '%typed-value))
	  (b-var (make-instance '%typed-value))
	  (c-var (make-instance '%typed-value)))
      (setf (%type a-var) 'a)
      (setf (%type b-var) 'b)
      (setf (%type c-var) 'c)
      (setf (%type-list or-a) '(a b))
      (%ensure-type or-a a-var)
      (%ensure-type or-a b-var)
      #+nil(%ensure-type or-a c-var)))
  "test finished")

;;;;;;;;;; end test 0 ;;;;;;;;;;;;;;

(defun assert-is-stack (stack)
  (assert (subtypep (type-of stack) '%typed-stack)))

(defun %push-empty (stack)
  ;; push some sort of empty indicator onto the stack
  (assert-is-stack stack)
  (let ((eltype (%element-type stack)))
    (let ((obj (make-instance eltype)))
      (push obj (%stack stack))
      stack)))

(defun %output (input-stack output-stack)
  ;; "return" the top item on the input-stack by pushing it onto the output
  ;; stack
  ;; no need to type-check, since DSL guarantees that the input and output types
  ;; are the same
  (assert (subtypep (type-of input-stack) '%typed-stack))
  (assert (subtypep (type-of output-stack) '%typed-stack))
  (let ((v (%top input-stack)))
    (%push output-stack v)
    output-stack))

(defun %pop (stack)
  (assert (subtypep (type-of stack) '%typed-stack))
  (pop (%stack stack))
  stack)

(defun %push (stack v)
  (assert (subtypep (type-of stack) '%typed-stack))
  (push v (%stack stack))
  stack)

(defun %top (stack)
  (when (null stack)
    (error "internal error 3: stack is empty"))
  (assert (subtypep (type-of stack) '%typed-stack))
  (if (< (length (%stack stack)) 1)
      (error "~&stack empty ~s~%" stack))
  (first (%stack stack)))

;; for debug...
(defun %ntop (stack)
  (first (%stack stack)))
;;

(defun %get-field (obj field-name)
  ;; return obj.field
  (slot-value obj field-name))

(defun %set-field (obj field-name val)
  ;; obj.field := val
  (setf (slot-value obj field-name) val))

(defmethod %append ((self %bag) (new-val %typed-value))
  ;(setf (lis self) (append (lis self) (list new-val)))
  (push new-val (lis self))
  self)

(defmethod %append ((self %map) (new-val %typed-value))
  (setf (ordered-list self) (append (ordered-list self) (list new-val)))
  self)

;;;;;;;;;; test 2 ;;;;

(defclass machineDescriptor-type (stack-dsl::%typed-value)
  ((%field-type-pipeline :accessor %field-type-pipeline :initform 'pipeline-type)
   (pipeline :accessor pipeline)
   (%field-type-statesBag :accessor %field-type-statesBag :initform 'statesBag-type)
   (statesBag :accessor statesBag)
   (%field-type-initiallyDescriptor :accessor %field-type-initiallyDescriptor :initform 'initiallyDescriptor-type)
   (initiallyDescriptor :accessor initiallyDescriptor)
   (%field-type-name :accessor %field-type-name :initform 'name-type)
   (name :accessor name)
   ))

(defclass machineDescriptor-stack (stack-dsl::%typed-stack) ())

(defmethod initialize-instance :after ((self machineDescriptor-stack) &key &allow-other-keys)
  (setf (stack-dsl::%element-type self) 'machineDescriptor-type))


(defclass name-type (stack-dsl::%string) ())
(defclass name-stack (stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self name-stack) &key &allow-other-keys)
  (setf (stack-dsl::%element-type self) 'name-type))



(defparameter *input-s* nil)
(defparameter *output-s* nil)
(defparameter *var* nil)

(defun test2-stack-dsl ()
  (let ((input-s (make-instance '%typed-stack :element-type 'machineDescriptor-type))
	(output-s (make-instance '%typed-stack :element-type 'machineDescriptor-type)))
    (%push-empty input-s)
    (format nil "length input stack = ~a, output stack = ~a" 
	    (length (%stack input-s)) (length (%stack output-s)))
    #+nil(progn
      (%output input-s output-s)
      (format nil "length input stack = ~a, output stack = ~a" 
	      (length (%stack input-s)) (length (%stack output-s))))
    #+nil(progn
	   (%output input-s output-s)
	   (%pop input-s)
	   (format nil "length input stack = ~a, output stack = ~a" 
		   (length (%stack input-s)) (length (%stack output-s)))
	   ;; use inspector to examine these values
	   (setf *input-s* input-s)
	   (setf *output-s* output-s))
    #+nil(let ((var-a (make-instance 'machineDescriptor-type)))
      (%replace-top input-s var-a)
      (%output input-s output-s)
      (%pop input-s)
      ;; use inspector to examine these values
      (setf *input-s* input-s)
      (setf *output-s* output-s)
      
      (format *standard-output* "~& top of output the same as var-a? ~a~%" (eq (%top output-s) var-a)))

    #+nil(let ((var-a (make-instance 'machineDescriptor-type)))
      (%replace-top input-s var-a)
      (let ((n (make-instance 'name-type)))
	(setf (%value n) "abc")
	(%set-field (%top input-s) 'name n)
	(%output input-s output-s)
	(%pop input-s)
	;; use inspector to examine these values
	(setf *input-s* input-s)
	(setf *output-s* output-s)
	(setf *var* (%get-field (%top output-s) 'name))
	
	(format *standard-output* "~& top of output the same as var-a? ~a~%" (eq (%top output-s) var-a)))
      )
    (let ((md (make-instance 'machineDescriptor-type))
	  (bag-a (make-instance '%bag :element-type 'machineDescriptor-type)))
      (%replace-top input-s bag-a)
      (%append (%top input-s) md)
      (%output input-s output-s)
      ;; use inspector to examine these values
      (setf *input-s* input-s)
      (setf *output-s* output-s)
      (setf *var* md))
    )
  )

;;;;;;;;;; end test 2 ;;;;
