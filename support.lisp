(in-package :stack-dsl)


;; stack of typed values
;; 2 stacks for each type - "input" and "output"

(defun %type-check-failure (expected val)
  (error (format nil "~%expected type ~a, but got ~a~%" expected val)))

(defun %type-check-failure-format (fmtstr &rest args)
  (let ((msg (apply 'format nil fmtstr args)))
    (error msg)))

;; all items must be %typed-value or %typed-struct or %typed-stack or %bag or %map or %string
(defclass %typed-value ()
  ((%type :accessor %type :initform :no-type :initarg :%type)
   (%value   :accessor %value   :initform :no-value)))

(defclass %compound-type (%typed-value)
  ((%type-list :accessor %type-list :initform nil :initarg :type-list)))
  
(defclass %typed-stack ()
  ((%element-type :accessor %element-type :initform :no-type :initarg :element-type)
   (%stack :accessor %stack :initform nil)))

(defclass %bag (%typed-value)
  ((%bag-element-type :accessor %bag-element-type :initform :no-type :initarg :bag-element-type)
   (lis :accessor lis :initform nil)))

(defclass %map (%typed-value)
  ((%map-element-type :accessor %map-element-type :initform :no-type :initarg :map-element-type)
   (ordered-list :accessor ordered-list :initform nil)))

(defclass %string (%typed-value)
  ()
  (:default-initargs 
   :%type "STRING-TYPE"))

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


;;;;;;;;;;;; type checking - type descriptors


(defparameter *type-table* nil)  ;; global only for debug
(defparameter *type-hash* nil)   ;; global only for debug

(defun make-types-from-string-list (str-list)
  (mapcar #'make-type-from-string str-list))

(defun make-type-from-string (s)
  s)

(defun string-member (s lis)
  ;; does this need to be more efficient?  (for small dsl's, we don't really care)
  (dolist (str lis)
    (when (string= str s)
      (return-from string-member T)))
  nil)


(defclass type-descriptor () 
  ((descriptor-alist :accessor descriptor-alist :initarg :descriptor-alist)))

(defclass string-descriptor (type-descriptor)
  ())
(defclass map-descriptor (type-descriptor)
  ((element-type :accessor element-type :initarg :element-type)))
(defclass bag-descriptor (type-descriptor)
  ((element-type :accessor element-type :initarg :element-type)))
(defclass enum-descriptor (type-descriptor)
  ((value-list :accessor value-list :initform nil :initarg :value-list)))
(defclass compound-descriptor (type-descriptor)
  ((types :accessor types :initform nil :initarg :types)))
(defclass structure-descriptor (type-descriptor)
  ((fields :accessor fields :initform nil :initarg :fields)))

(defun create-string-descriptor (adesc)
  (make-instance 'string-descriptor :descriptor-alist adesc))

(defun create-map-descriptor (adesc)
  (let ((eltype (intern (string-upcase (cdr (assoc :element-type adesc))) "KEYWORD")))
    (make-instance 'map-descriptor 
		   :descriptor-alist adesc
		   :element-type eltype)))
  
(defun create-bag-descriptor (adesc)
  (let ((eltype (intern (string-upcase (cdr (assoc :element-type adesc))) "KEYWORD")))
    (make-instance 'map-descriptor 
		   :descriptor-alist adesc
		   :element-type eltype)))
  
(defun create-enum-descriptor (adesc)
  (let ((values-string-list (cdr (assoc :value-list adesc))))
    (let ((values-keyword-list nil))
      (dolist (str values-string-list)
	(push (intern (string-upcase str) "KEYWORD")
	      values-keyword-list))
      (make-instance 'enum-descriptor
		     :descriptor-alist adesc
		     :value-list values-keyword-list))))

(defun create-compound-descriptor (adesc)
  (let ((string-list (cdr (assoc :types adesc))))
    (let ((types-list (make-types-from-string-list string-list)))
      (make-instance 'compound-descriptor
		     :descriptor-alist adesc
		     :types types-list))))

(defun create-structure-descriptor (adesc)
  (let ((alist-name-type-pairs (cdr (assoc :fields adesc))))
    ;; (( (:FIELD-NAME . "exprkind") (:FIELD-TYPE . "exprkind") )...)
    (make-instance 'structure-descriptor
		   :descriptor-alist adesc
		   :fields alist-name-type-pairs)))
  
(defun initialize-type-hash (type-hash type-table)
  (dolist (a type-table)
    (let ((tyname (make-type-from-string (cdr (assoc :name a))))
	  (adesc (cdr (assoc :descriptor a))))
      (let ((kind-sym (intern (string-upcase (cdr (assoc :kind adesc))) "KEYWORD")))
	(let ((desc
	       (ecase kind-sym
		 (:string (create-string-descriptor adesc))
		 (:enum (create-enum-descriptor adesc))
		 (:structure (create-structure-descriptor adesc))
		 (:compound (create-compound-descriptor adesc))
		 (:map (create-map-descriptor adesc))
		 (:bag (create-bag-descriptor adesc)))))
	  (setf (gethash tyname type-hash) desc)))))
  type-hash)


(defmethod deep-type-equal ((self T) (obj T))
  nil)
(defmethod deep-type-equal ((self string-descriptor) (obj string-descriptor))
  T)
(defmethod deep-type-equal ((self map-descriptor) (obj map-descriptor))
  (eq (element-type self) (element-type obj)))
(defmethod deep-type-equal ((self bag-descriptor) (obj bag-descriptor))
  (eq (element-type self) (element-type obj)))
(defmethod deep-type-equal ((self enum-descriptor) (obj enum-descriptor))
  (let ((self-values (values self))
	(obj-values (values obj)))
    (and (= (length self-values) (length obj-values))
	 (every #'(lambda (x) (string-member x obj-values)) self-values))))
(defmethod deep-type-equal ((self compound-descriptor) (obj compound-descriptor))
  (let ((self-types (types self))
	(obj-types (types obj)))
    (and (= (length self-types) (length obj-types))
	 (every #'(lambda (x) (string-member x obj-types)) self-types))))
(defmethod deep-type-equal ((self structure-descriptor) (obj structure-descriptor))
  (let ((self-fields (fields self))
	(obj-fields (fields obj)))
    (and (= (length self-fields) (length obj-fields))
	 (every #'(lambda (x) (string-member x obj-fields)) self-fields))))

(defmethod shallow-type-equal ((self T) (obj T))
  nil)
(defmethod shallow-type-equal ((self %typed-value) (obj %typed-value))
  (string= (%type self) (%type obj)))

(defun lookup-type (name)
  (multiple-value-bind (descriptor success)
      (gethash name *type-hash*)
    (if (not success)
	nil
	descriptor)))

(defun lookup-type-or-fail (name)
  (multiple-value-bind (descriptor success)
      (gethash name *type-hash*)
    (if (null success)
	(%type-check-failure-format "type ~a is not defined" name)
	(if (null descriptor)
	    (%type-check-failure "type ~a cannot not found" name)
	    descriptor))))

(defun find-field-type (pairs field-name)
  (dolist (p pairs) ;; ((:field-name . "exprKind") (:field-type . "exprKind"))
    (let ((name (cdr (assoc :field-name p)))
	  (ty   (cdr (assoc :field-type p))))
      (assert (stringp name))
      (assert (stringp ty))
      (when (string= field-name name)
	(return-from find-field-type ty))))
  nil)

(defmethod lookup-field-type (type-desc field-name)
  nil)
(defmethod lookup-field-type ((type-desc structure-descriptor) field-name)
  (let ((field-pairs (fields type-desc)))
    (if (null field-pairs)
	(%type-check-failure-format "internal error lookup-field-type ~s ~s"
				    type-desc field-name)
	(let ((field-type (find-field-type field-pairs field-name)))
	  (if (null field-type)
	      nil
	      field-type)))))
  
(defun lookup-field-type-or-fail (type-name field-name)
  (let ((main-desc (lookup-type-or-fail type-name)))
    (let ((field-type-name (lookup-field-type main-desc field-name)))
      (if field-type-name
	  (let ((field-desc (lookup-type-or-fail field-type-name)))
	    (if field-desc
		field-desc
		(%type-check-failure-format "typo? type ~a does not have a field ~a" type-name field-name)))
	  (%type-check-failure-format "type ~a does not have a field ~a" type-name field-name)))))

;; type checking

(defun read-json-types (filename)
  (alexandria:read-file-into-string filename))

(defun types-as-alist (filename)
  (let ((str (read-json-types filename)))
    (with-input-from-string (s str)
      (json:decode-json s))))

(defun initialize-types (filename)
  (setf *type-table* (types-as-alist filename))
  (setf *type-hash* (make-hash-table :test 'equal))
  (initialize-type-hash *type-hash* *type-table*))
 
    
(defmethod %ensure-type (expected-type (obj T))
  (%type-check-failure-format "expected type must be a type descriptor, object must be  %typed-value"))

(defmethod %ensure-type (expected-type (obj %typed-value))
  ;; return T if type checks out, else %type-check-failure
  (let ((expected-type-desc (lookup-type-or-fail expected-type)))
    (let ((obj-desc (lookup-type-or-fail (%type obj))))
      (shallow-type-equal expected-type-desc obj-desc))))

(defmethod %ensure-field-type ((self T) field-name (obj T))
  (%type-check-failure (format nil "~a has no field called ~a" self field-name) obj))

(defmethod %ensure-field-type (expected-type field-name (obj %typed-value))
  (let ((expected-type-desc (lookup-type-or-fail expected-type)))
    (unless expected-type-desc 
      (%type-check-failure "not a structure" expected-type))
    (let ((obj-type-desc (lookup-type-or-fail (%type obj))))
      (unless obj-type-desc 
	(%type-check-failure "not a structure" obj))
      (let ((field-type-desc (lookup-field-type-or-fail expected-type field-name)))
	(shallow-type-equal field-type-desc obj-type-desc)))))

;;;;;;;;;;;; end type checking




(defun assert-is-stack (stack)
  (assert (subtypep (type-of stack) '%typed-stack)))

(defun lisp-sym (str)
  (intern (string-upcase str)))

(defun %push-empty (stack)
  ;; push some sort of empty indicator onto the stack
  (assert-is-stack stack)
  (let ((eltype (%element-type stack)))
    (assert (stringp eltype))
    (let ((clss (lisp-sym eltype)))
      (let ((obj (make-instance clss :%type eltype)))
	(push obj (%stack stack))
	stack))))

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

(defun %lookup-field (obj field-name)
  ;; return obj.field
  (slot-value obj (lisp-sym field-name)))

(defun %set-field (obj field-name val)
  ;; obj.field := val
  (setf (slot-value obj (lisp-sym field-name)) val))

(defmethod %append ((self %bag) (new-val %typed-value))
  ;(setf (lis self) (append (lis self) (list new-val)))
  (push new-val (lis self))
  self)

(defmethod %append ((self %map) (new-val %typed-value))
  (setf (ordered-list self) (append (ordered-list self) (list new-val)))
  self)


;;;;;;;;;; dev tests ;;;;


(defclass machineDescriptor (stack-dsl::%typed-value)
  ((%field-type-pipeline :accessor %field-type-pipeline :initform "pipeline")
   (pipeline :accessor pipeline)
   (%field-type-statesBag :accessor %field-type-statesBag :initform "statesBag")
   (statesBag :accessor statesBag)
   (%field-type-initiallyDescriptor :accessor %field-type-initiallyDescriptor :initform "initiallyDescriptor")
   (initiallyDescriptor :accessor initiallyDescriptor)
   (%field-type-name :accessor %field-type-name :initform "name")
   (name :accessor name)
   ) (:default-initargs :%type "machineDescriptor"))

(defclass machineDescriptor-stack (stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self machineDescriptor-stack) &key &allow-other-keys)
  (setf (stack-dsl::%element-type self) "machineDescriptor"))

(defclass initiallyDescriptor (stack-dsl::%bag) () (:default-initargs :%type "initiallyDescriptor"))
(defmethod initialize-instance :after ((self initiallyDescriptor) &key &allow-other-keys)  ;; type for items in bag
	   (setf (stack-dsl::%bag-element-type self) "initiallyDescriptor"))
(defclass initiallyDescriptor-stack(stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self initiallyDescriptor-stack) &key &allow-other-keys)
	   (setf (stack-dsl::%element-type self) "initiallyDescriptor"))

(defclass statesBag (stack-dsl::%bag) () (:default-initargs :%type "statesBag"))
(defmethod initialize-instance :after ((self statesBag) &key &allow-other-keys)  ;; type for items in bag
	   (setf (stack-dsl::%bag-element-type self) "statesBag"))
(defclass statesBag-stack(stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self statesBag-stack) &key &allow-other-keys)
	   (setf (stack-dsl::%element-type self) "statesBag"))
(defclass state (stack-dsl::%typed-value)
  ((%field-type-eventsBag :accessor %field-type-eventsBag :initform "eventsBag")
   (eventsBag :accessor eventsBag)
   (%field-type-name :accessor %field-type-name :initform "name")
   (name :accessor name)
   ) (:default-initargs :%type "state"))

(defclass name (stack-dsl::%string) () (:default-initargs :%type "name"))
(defclass name-stack (stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self name-stack) &key &allow-other-keys)
  (setf (stack-dsl::%element-type self) "name"))

(defclass pipeline (stack-dsl::%map) () (:default-initargs :%type "pipeline"))
(defmethod initialize-instance :after ((self pipeline) &key &allow-other-keys)  ;; type for items in map
	   (setf (stack-dsl::%map-element-type self) "pipeline"))
(defclass pipeline-stack(stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self pipeline-stack) &key &allow-other-keys)
	   (setf (stack-dsl::%element-type self) "pipeline"))

(defparameter *input-s* nil)
(defparameter *output-s* nil)
(defparameter *var* nil)

;;;; test 0 ;;;;

(defun test-stack-dsl ()
  (initialize-types (asdf:system-relative-pathname :stack-dsl "types.json"))
  #+nil(%ensure-type 5 6)
  (let ((x (make-instance '%typed-value :%type "name")))
    #+nil(%ensure-type x 7)
    (%ensure-type "name" x))
  #+nil(let ((y (make-instance '%typed-value :%type "a")))
    (%ensure-type "name" y))
  "test finished")

;;;;;;;;;; end test 0 ;;;;;;;;;;;;;;

(defun test2-stack-dsl ()
  (let ((input-s (make-instance '%typed-stack :element-type 'machineDescriptor))
	(output-s (make-instance '%typed-stack :element-type 'machineDescriptor)))
    (%push-empty input-s)
    (format nil "length input stack = ~a, output stack = ~a" 
	    (length (%stack input-s)) (length (%stack output-s)))

    (let ((var-md (make-instance 'machineDescriptor)))
      (let ((input-md (make-instance 'machineDescriptor-stack :element-type 'machineDescriptor))
	    (output-md (make-instance 'machineDescriptor-stack :element-type 'machineDescriptor))
	    (nm (make-instance 'name))
	    (input-name (make-instance 'name-stack :element-type 'name))
	    (output-name (make-instance 'name-stack :element-type 'name)))
	(setf (%value nm) "abc")
	(%push input-name nm)
	(%output input-name output-name)
	(%push input-md var-md)
	
        (%set-field (%top input-md) "name" (%top output-name))
	(%pop output-name)
	
	(%output input-md output-md)
	(%pop input-md)
	;; use inspector to examine these values
	(setf *input-s* input-md)
	(setf *output-s* output-md)))
    ))

;;;;;;;;;; end test 2 ;;;;
