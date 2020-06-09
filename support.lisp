(in-package :stack-dsl)

(defparameter *target-package* "CL-USER")

(defun set-target-package (str)
  (setf *target-package* str))

;; stack of typed values
;; 2 stacks for each type - "input" and "output"

(defun %type-check-failure (expected val)
  (error (format nil "~%expected type ~a, but got ~a~%" expected val)))

(defun %type-check-failure-format (fmtstr &rest args)
  (let ((msg (apply 'format nil fmtstr args)))
    (error msg)))


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


(defun field-equal (field1 field2)
  (and (eql (car field1) (car field2))
       (eql (cdr field1) (cdr field2))))

(defun field-member (actual-field field-list)
  (dolist (f field-list)
    (when (field-equal actual-field f)
      (return-from field-member t)))
  nil)

 
;; type checking
    
(defmethod %ensure-type (expected-type (obj T))
  (%type-check-failure-format "expected type must be a type descriptor, object must be  %typed-value"))

(defmethod %ensure-type (expected-type (val STRING))
  ;; return T if type checks out, else %type-check-failure
  (let ((expected-type-desc (lookup-type-or-fail expected-type)))
    (unless (eq 'enum-descriptor (type-of expected-type-desc))
      (%type-check-failure-format "expected type must be an enum, object must be a string"))
    (unless (deep-type-equal expected-type-desc val)
      (%type-check-failure-format "%s is not an allowable value for enum ~s" val expected-type))))

(defmethod %ensure-type (expected-type (obj %typed-value))
  ;; return T if type checks out, else %type-check-failure
  (let ((expected-type-desc (lookup-type-or-fail expected-type)))
    (let ((obj-desc (lookup-type-or-fail (%type obj))))
      (deep-type-equal expected-type-desc obj-desc))))

(defmethod %ensure-appendable-type ((obj T))
  (%type-check-failure-format "type ~a must be a bag or a map, but is not"
			      obj))

(defmethod %ensure-appendable-type ((obj %bag))
  T)

(defmethod %ensure-appendable-type ((obj %map))
  T)

(defmethod %ensure-appendable-type ((collection-type STRING))
  (let ((type-desc collection-type))
    (if (or (eq (type-of type-desc) 'map-descriptor)
	    (eq (type-of type-desc) 'bag-descriptor))
	:ok
	(%type-check-failure-format "type ~a is expected to be a bag or a map"
				    (%element-type stack))))
  T)

#+nil(defmethod %ensure-appendable-type ((stack %typed-stack))
  (let ((type-desc (lookup-type-or-fail (%element-type stack))))
    (if (or (eq (type-of type-desc) 'map-descriptor)
	    (eq (type-of type-desc) 'bag-descriptor))
	:ok
	(%type-check-failure-format "type ~a is expected to be a bag or a map"
				    (%element-type stack))))
  T)

(defmethod %ensure-field-type ((self T) field-name (obj T))
  (%type-check-failure-format (format nil "~a has no field called ~a" self field-name)))

(defmethod %ensure-field-type ((expected-type-name STRING) (field-name STRING) (obj %typed-value))
  ;; struct-type-desc = lookup-type-desc(expect-type)
  ;; field-type-desc = struct-type-desc.get-field(field-name)
  ;; error if not-found(field-type-desc)
  ;; obj-type-desc = lookup-type-desc(obj)
  ;; error if not-found(obj-type-desc)
  ;; error if not (obj-type-desc shallow-equal field-type-desc)
  (let ((struct-type-desc (lookup-type-or-fail expected-type-name)))
    (unless struct-type-desc 
      (%type-check-failure "~s is not a known type" expected-type-name))
    (let ((field-type-desc (lookup-field-type-or-fail expected-type-name field-name)))
      (unless field-type-desc
	(%type-check-failure-format "~s does not have a field ~s" expected-type-name field-name))
      (let ((obj-type-desc (lookup-field-type-or-fail expected-type-name field-name)))
	(unless obj-type-desc
	  (%type-check-failure "~s does not have a known type (it must be a %typed-value or a STRING (enum value)" obj))	  
	(unless (deep-type-equal field-type-desc obj-type-desc)
	  (%type-check-failure-format "~s (with type ~s) does not have the same type as ~s.~s" 
				      obj
				      (%name obj-type-desc)
				      expected-type-name
				      field-name))))))

(defmethod %ensure-field-type ((expected-type-name STRING) (field-name STRING) (val STRING))
  ;; if val is a STRING, then expected-type.field MUST be an enum and val must be a valid enum tag for that enum
  (let ((struct-type-desc (lookup-type-or-fail expected-type-name)))
    (unless struct-type-desc 
      (%type-check-failure "~s is not a known type" expected-type-name))
    (let ((field-type-desc (lookup-field-type-or-fail expected-type-name field-name)))
      (unless field-type-desc
	(%type-check-failure-format "~s does not have a field ~s" expected-type-name field-name))
      (unless (enum-p field-type-desc)
	(%type-check-failure-format "field ~s of type ~s, must be an enum" field-name expected-type-name))
      (unless (valid-enum-tag-p field-type-desc val)
	(%type-check-failure-format "~s is not a valid tag for field ~s of enum ~s" 
				    val
				    field-name
				    expected-type-name)))))


;;;;;;;;;;;; end type checking




(defun assert-is-stack (stack)
  (assert (subtypep (type-of stack) '%typed-stack)))

(defun lisp-sym (str)
  (intern (string-upcase str) *target-package*))

(defun %push-empty (stack)
  ;; push some sort of empty indicator onto the stack
  (assert-is-stack stack)
  (let ((eltype (%element-type stack)))
    (assert (stringp eltype))
    (let ((clss (lisp-sym eltype)))
      (let ((obj (make-instance clss :%type eltype)))
	(push obj (%stack stack))
	stack))))

(defun %output (output-stack input-stack)
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

(defun %set-top (stack v)
  (assert (subtypep (type-of stack) '%typed-stack))
  (setf (first (%stack stack)) v)
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
  (let ((cl-field-name (lisp-sym field-name)))
    (setf (slot-value obj cl-field-name) val)))

(defmethod %append ((self %bag) (new-val %typed-value))
  ;(setf (lis self) (append (lis self) (list new-val)))
  (pushnew new-val (lis self))
  self)

(defmethod %append ((self %map) (new-val %typed-value))
  (setf (%ordered-list self) (append (%ordered-list self) (list new-val)))
  self)


;; various accessors for built-in types

(defmethod %list ((self %map))
  (%ordered-list self))

(defmethod %list ((self %bag))
  (lis self))

(defmethod %as-string ((self %string))
  (%value self))

(defmethod %as-string ((self %enum))
  (%value self))

;;;; macros ;;;;


;;
;; There is no need to read the actual Lisp macros below.
;; The macros, in effect, rewrite text into other text (over-simplification).
;;
;; I list the effect of each macro, in loose terms, below...:

;; there are 2 stacks for every type
;;  [one stack is called "input" and the other is called "output" (corresponding to the input parameter list and return value)]

;; 6 operations on the 2 kinds of stacks:
;;
;; output
;; newscope
;; replace-top
;; append
;; pop-output
;; set-field

;; loosey-goosey meanings...
;;
;; output(type) : return value given by type of stack "input"
;; newscope(type) : push <nothing-ness> onto input stack of type
;; replace-top(x,y) : replace top x item with y
;; append(x,y) : append y to top x
;; pop-ouput(x) : kill top x item
;; set-field (x f val) : assign val to x.f

;; loosey-goosey semantics...
;;
;; output(type) : move top input item to top of output stack ; move top(type) from input-type to output-type, pop input-type
;; newscope(type) : push <nothing-ness> onto input stack of type
;; replace-top(x,y) : replace top x item with y ; set top(x) to be top(y) (replace, not push), pop top(y), check that replacement type is the same as the replacee
;; append(x,y) : append y to top x ; append top(y) to top(x), check that type of top(y) is append-able to top(x)
;; pop-ouput(x) : kill top x item ; return top(x), pop x
;; set-field (x f val) : top(x).f <- val where top(x) must have field f, val must be of type compatible with type top(x).f


;; I hard-code the symbols "self" and "env" ...
;; I use ~ and % as name prefixes for names.
;; ~ is used to prefix macro names.
;; % is used to prefix support routines.
;; (I could have used Common Lisp packages to qualify names instead of prefixes, but I thought that prefixes are less scary and show (visually) my design intentions).

(defun ~in(name) `(,(intern (string-upcase (format nil "input-~a" name))) (env self)))
(defun ~out(name) `(,(intern (string-upcase (format nil "output-~a" name))) (env self)))

(defun ~field(fname) (format nil "%field-type-~s" fname))
(defun ~type(fname) (stack-dsl:lisp-sym fname))

(defmacro ~output (ty)
  `(progn 
     (stack-dsl:%output ,(~out ty) ,(~in ty))
     (stack-dsl:%pop ,(~in ty))))

(defmacro ~newscope (ty)
  `(stack-dsl:%push-empty ,(~in ty)))

(defmacro ~append (stack1 stack2)
  `(let ((val (stack-dsl:%top ,(~out stack2))))
     (stack-dsl:%ensure-appendable-type ,(~in stack1))
     (stack-dsl:%ensure-type (stack-dsl:%element-type 
			      (stack-dsl:%top ,(~in stack1))) val)
     (stack-dsl::%append (stack-dsl:%top ,(~in stack1)) val)
     (stack-dsl:%pop ,(~out stack2))))

(defmacro ~set-field (to field-name from)
  ;; set top(input-to).f := output-from, pop from
  `(let ((val (stack-dsl:%top ,(~out from))))
     (stack-dsl:%ensure-field-type
      ,to
      ,field-name
      val)
     (stack-dsl:%set-field (stack-dsl:%top ,(~in to)) ,field-name val)
     (stack-dsl:%pop ,(~out from))))

;;;;;; end macros

