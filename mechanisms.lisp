(in-package :stack-dsl)


(defun intern-all (list-of-strings)
  (let ((r (mapcar #'(lambda(s) (intern (string-upcase s))) list-of-strings)))
    r))

;; mechanisms
(defmethod errorTail ((self stack-dsl-parser)) (error "tail error"))
(defmethod errorColonTail ((self stack-dsl-parser)) (error "colon tail error"))

(defmethod nullEmit ((self stack-dsl-parser))
  (let ((tyname (savedSymbol self)))
    (pasm:emit-string self "
(defclass ~a (stack-dsl::%null) () (:default-initargs :%type \"~a\"))
(defclass ~a-stack (stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self ~a-stack) &key &allow-other-keys)
  (setf (stack-dsl::%element-type self) \"~a\"))

"
  tyname tyname tyname tyname tyname)))

(defmethod stringEmit ((self stack-dsl-parser))
  (let ((tyname (savedSymbol self)))
    (pasm:emit-string self "
(defclass ~a (stack-dsl::%string) () (:default-initargs :%type \"~a\"))
(defclass ~a-stack (stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self ~a-stack) &key &allow-other-keys)
  (setf (stack-dsl::%element-type self) \"~a\"))

"
  tyname tyname tyname tyname tyname)))

(defmethod bagEmit ((self stack-dsl-parser))
  (let ((element-type-name (scanner:token-text (pasm:accepted-token self)))
	(tyname (savedSymbol self)))
    (pasm:emit-string self "(defclass ~a (stack-dsl::%bag) () (:default-initargs :%type \"~a\"))~%" tyname tyname)
    (pasm:emit-string self "(defmethod initialize-instance :after ((self ~a) &key &allow-other-keys)  ;; type for items in bag~%" tyName)
    (pasm:emit-string self "(setf (stack-dsl::%element-type self) \"~a\"))~%" tyName)
    (pasm:emit-string self "(defclass ~a-stack(stack-dsl::%typed-stack) ())~%" tyName)
    (pasm:emit-string self " (defmethod initialize-instance :after ((self ~a-stack) &key &allow-other-keys)~%" tyName)
    (pasm:emit-string self "(setf (stack-dsl::%element-type self) \"~a\"))~%" tyName)))

(defmethod mapEmit ((self stack-dsl-parser))
  (let ((element-type-name (scanner:token-text (pasm:accepted-token self)))
	(tyname (savedSymbol self)))
    (pasm:emit-string self "(defclass ~a (stack-dsl::%map) () (:default-initargs :%type \"~a\"))~%" tyname tyname)
    (pasm:emit-string self "(defmethod initialize-instance :after ((self ~a) &key &allow-other-keys)  ;; type for items in map~%" tyName)
    (pasm:emit-string self "(setf (stack-dsl::%element-type self) \"~a\"))~%" tyName)
    (pasm:emit-string self "(defclass ~a-stack(stack-dsl::%typed-stack) ())~%" tyName)
    (pasm:emit-string self " (defmethod initialize-instance :after ((self ~a-stack) &key &allow-other-keys)~%" tyName)
    (pasm:emit-string self "(setf (stack-dsl::%element-type self) \"~a\"))~%" tyname)))

(defmethod compoundPushNew ((self stack-dsl-parser)) (setf (compound-list self) nil))
(defmethod compoundAddSymbol ((self stack-dsl-parser)) 
  (push (scanner:token-text (pasm:accepted-token self)) (compound-list self)))
(defmethod compoundEmit ((self stack-dsl-parser))
    (let ((tyname (savedSymbol self)))
      (pasm:emit-string self "
(defclass ~a (stack-dsl::%compound-type) () (:default-initargs :%type \"~a\"))
(defmethod initialize-instance :after ((self ~a) &key &allow-other-keys)
  (setf (stack-dsl::%type-list self) '~s))
(defclass ~a-stack (stack-dsl::%typed-stack) () (:default-initargs :%element-type \"~a\"))

"
			tyname tyname tyname (compound-list self)
			tyname tyname)))

(defmethod fieldClear ((self stack-dsl-parser))
  (setf (field-list self) nil))
(defmethod fieldPushNew ((self stack-dsl-parser))
  (let ((field-name (scanner:token-text (pasm:accepted-token self))))
    (push (cons field-name nil) (field-list self))))
(defmethod fieldSetInit ((self stack-dsl-parser))
  (let ((init (scanner:token-text (pasm:accepted-token self))))
    (setf (first (field-list self))
	  (cons (first (first (field-list self))) init))))

(defmethod fieldEmit ((self stack-dsl-parser))
  (let ((tyName (savedSymbol self)))
    (emit-string self "(defclass ~a (stack-dsl::%typed-value)~%(" tyName)
    (dolist (f (field-list self))
      (let ((field-name (first f))
	    (init (cdr f)))
	(emit-string self "(%field-type-~a :accessor %field-type-~a :initform \"~a\")~%"
		     field-name field-name field-name)
        (if (null init)
	    (emit-string self "(~a :accessor ~a)~%" field-name field-name)
	    (emit-string self "(~a :accessor ~a :initform '~a)" field-name field-name init))))
    (emit-string self ") (:default-initargs :%type \"~a\"))
" tyName)
    (emit-string self "
(defclass ~a-stack (stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self ~a-stack) &key &allow-other-keys)~%  (setf (stack-dsl::%element-type self) \"~a\"))

" tyName tyName tyName)))

(defmethod existenceTypeSave ((self stack-dsl-parser))
  (let ((tyName (scanner:token-text (pasm:accepted-token self))))
    (push-existence-new (existence-list self) tyName)))
(defmethod existenceEmit ((self stack-dsl-parser))
  (pasm:emit-string self "~%~%;; check forward types~%")
  (map-existence-list (existence-list self)
   #'(lambda (ty)
       (pasm:emit-string self "(stack-dsl::%ensure-existence '~a)~%" ty))))

(defmethod symbolSave ((self stack-dsl-parser))
  (setf (savedSymbol self) (scanner:token-text (pasm:accepted-token self))))

(defmethod headerEmit ((self stack-dsl-parser))
  (emit-string self "(in-package ~s)~%~%" (target-package self)))

(defmethod environmentEmit ((self stack-dsl-parser))
  (pasm:emit-string self "~%(defclass environment ()~%((%water-mark :accessor %water-mark :initform nil)~%")
  (map-existence-list (existence-list self)
    #'(lambda (tyName)
      (pasm:emit-string self "(input-~a :accessor input-~a :initform (make-instance '~a-stack))~%" tyName tyName tyName)
      (pasm:emit-string self "(output-~a :accessor output-~a :initform (make-instance '~a-stack))~%" tyName tyName tyName)))
  (pasm:emit-string self "))~%~^%")
  (pasm:emit-string self "~%(defmethod %memoStacks ((self environment))~%(setf (%water-mark self)~%(list~%")
  (map-existence-list 
   (existence-list self)
   #'(lambda (tyName)
       (pasm:emit-string self "(input-~a self)~%" tyName)
       (pasm:emit-string self "(output-~a self)~%" tyName)))
  (pasm:emit-string self ")))~%~%")  
  (pasm:emit-string self "(defmethod %memoCheck ((self environment))~%(let ((wm (%water-mark self)))~%(unless (and~%")
  (let ((counter 0))
    (map-existence-list 
     (existence-list self)
     #'(lambda (tyName)
	 (pasm:emit-string self "(eq (nth ~a wm) (input-~a self))~%" counter tyName)
	 (incf counter)
	 (pasm:emit-string self "(eq (nth ~a wm) (output-~a self))~%" counter tyName)
	 (incf counter)))
  (pasm:emit-string self "))~%(error \"stack depth incorrect\")))~%")))


(defmethod enumPushNew ((self stack-dsl-parser))
  (setf (enum-list self) nil))

(defmethod enumPushSymbol ((self stack-dsl-parser))
  (push (format nil "\"~a\"" (scanner:token-text (pasm:accepted-token self))) (enum-list self)))

(defmethod enumEmit ((self stack-dsl-parser))
  (let ((tyName (savedSymbol self)))
    (pasm:emit-string self "
(defclass ~a (%enum) () (:default-initargs :%type \"~a\"))
"                tyName tyName)
  (pasm:emit-string self "
(defmethod initialize-instance :after ((self ~a) &key &allow-other-keys)
  (setf (stack-dsl::%value-list self) '~a))

"              tyName (enum-list self))
  (pasm:emit-string self "
(defclass ~a-stack (stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self ~a-stack) &key &allow-other-keys)
  (setf (stack-dsl::%element-type self) \"~a\"))
"              tyName tyName tyName)))
    
;; end mechanisms

(defun upcase-and-suffix-with-type (str-list)
  (mapcar #'(lambda (str)
	      (format nil "~a" str))
	  str-list))
	  
