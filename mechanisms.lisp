(in-package :stack-dsl)


(defun intern-all (list-of-strings)
  (let ((r (mapcar #'(lambda(s) (intern (string-upcase s))) list-of-strings)))
    r))

;; mechanisms
(defmethod errorTail ((self stack-dsl-parser)) (error "tail error"))
(defmethod errorColonTail ((self stack-dsl-parser)) (error "colon tail error"))

(defmethod stringEmit ((self stack-dsl-parser))
  (let ((tyname (savedSymbol self)))
    (pasm:emit-string self "
(defclass ~a-type (stack-dsl::%string) ())
(defclass ~a-stack (stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self ~a-stack) &key &allow-other-keys)
  (setf (stack-dsl::%element-type self) '~a-type))

"
  tyname tyname tyname tyname)))

(defmethod bagEmit ((self stack-dsl-parser))
  (let ((target-name (scanner:token-text (pasm:accepted-token self)))
	(tyname (savedSymbol self)))
    (pasm:emit-string self "
(defclass ~a-type (stack-dsl::%typed-bag) ())
(defmethod initialize-instance :after ((self ~a-type) &key &allow-other-keys)  ;; type for items in bag
  (setf (stack-dsl::%element-type self) '~a))
(defclass ~a-stack(stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self ~a-stack) &key &allow-other-keys)
  (setf (stack-dsl::%element-type self) '~a-type))


"
 tyname tyname target-name tyname tyname tyname)))

(defmethod mapEmit ((self stack-dsl-parser))
  (let ((target-name (scanner:token-text (pasm:accepted-token self)))
	(tyname (savedSymbol self)))
    (pasm:emit-string self "
(defclass ~a-type (stack-dsl::%typed-map) ())
(defmethod initialize-instance :after ((self ~a-type) &key &allow-other-keys)  ;; type for items in bag
  (setf (stack-dsl::%type self) '~a))
(defclass ~a-stack (stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self ~a-stack) &key &allow-other-keys)
  (setf (stack-dsl::%element-type self) '~a-type))


"
 tyname tyname target-name tyname tyname tyname)))


(defmethod orPushNew ((self stack-dsl-parser)) (setf (or-list self) nil))
(defmethod orAddSymbol ((self stack-dsl-parser)) 
  (push (scanner:token-text (pasm:accepted-token self)) (or-list self)))
(defmethod orEmit ((self stack-dsl-parser))
    (let ((tyname (savedSymbol self)))
      (pasm:emit-string self "
(defclass ~a-type (stack-dsl::%or-type) ())
(defclass ~a-stack (stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self ~a-type) &key &allow-other-keys)
  (setf (stack-dsl::%type-list self) '~a))

"
			tyname tyname tyname (suffix-with-type (or-list self)))))

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
    (emit-string self "(defclass ~a-type (stack-dsl::%typed-value)~%(" tyName)
    (dolist (f (field-list self))
      (let ((field-name (first f))
	    (init (cdr f)))
	(emit-string self "(%field-type-~a :accessor %field-type-~a :initform '~a)~%"
		     field-name field-name field-name)
        (if (null init)
	    (emit-string self "(~a :accessor ~a)~%" field-name field-name)
	    (emit-string self "(~a :accessor ~a :initform '~a)" field-name field-name init))))
    (emit-string self "))
")
    (emit-string self "(defmethod initialize-instance :after ((self ~a-type) &key &allow-other-keys)~%" tyName)
    ;; type of each field is the same as name for each field
    (dolist (f (field-list self))
      (let ((field-name (first f)))
	(emit-string self "(setf (%type (~a self)) '~a-type)~%" field-name field-name)))
    (emit-string self ")
(defclass ~a-stack (stack-dsl::%typed-stack) ())
(defmethod initialize-instance :after ((self ~a-stack) &key &allow-other-keys)~%  (setf (stack-dsl::%element-type self) '~a-type))

" tyName tyName tyName)))

(defmethod existenceTypeSave ((self stack-dsl-parser))
  (let ((tyName (scanner:token-text (pasm:accepted-token self))))
    (cl:pushnew tyName (existence-list self))))
(defmethod existenceClear ((self stack-dsl-parser))
  (setf (existence-list self) nil))
(defmethod existenceEmit ((self stack-dsl-parser))
  (pasm:emit-string self "~%~%;; check forward types~%")
  (setf (existence-list self) (remove-duplicates (intern-all (existence-list self))))
  (dolist (ty (existence-list self))
    (pasm:emit-string self "(stack-dsl::%ensure-existence '~a-type)~%" ty)))

(defmethod symbolSave ((self stack-dsl-parser))
  (setf (savedSymbol self) (scanner:token-text (pasm:accepted-token self))))

(defmethod headerEmit ((self stack-dsl-parser))
  (emit-string self "(in-package ~s)~%~%" (target-package self)))

(defmethod environmentEmit ((self stack-dsl-parser))
  (pasm:emit-string self "~%(defclass environment ()~%((%water-mark :accessor %water-mark :initform nil)~%")
  (dolist (tyName (existence-list self))
    (pasm:emit-string self "(input-~a :accessor input-~a :initform (make-instance '~a-stack))~%" tyName tyName tyName)
    (pasm:emit-string self "(output-~a :accessor output-~a :initform (make-instance '~a-stack))~%" tyName tyName tyName))
  (pasm:emit-string self "))~%~^%")
  (pasm:emit-string self "~%(defmethod %memoStacks ((self environment))~%(setf (%water-mark self)~%(list~%")
  (dolist (tyName (existence-list self))
    (pasm:emit-string self "(input-~a self)~%" tyName)
    (pasm:emit-string self "(output-~a self)~%" tyName))
  (pasm:emit-string self ")))~%~%")
  
  (pasm:emit-string self "(defmethod %memoCheck ((self environment))~%(let ((wm (%water-mark self)))~%(unless (and~%")
  (let ((counter 0))
    (dolist (tyName (existence-list self))
      (pasm:emit-string self "(eq (nth ~a wm) (input-~a self))~%" counter tyName)
      (incf counter)
      (pasm:emit-string self "(eq (nth ~a wm) (output-~a self))~%" counter tyName)
      (incf counter)))
  (pasm:emit-string self "))~%(error \"stack depth incorrect\")))~%"))


(defmethod enumPushNew ((self stack-dsl-parser))
  (setf (enum-list self) nil))

(defmethod enumPushSymbol ((self stack-dsl-parser))
  (push (scanner:token-text (pasm:accepted-token self)) (enum-list self)))

(defmethod enumEmit ((self stack-dsl-parser))
  (let ((tyName (savedSymbol self)))
    (pasm:emit-string self "
(defclass ~a-type (%enum) () )
"                tyName)
  (pasm:emit-string self "
(defmethod initialize-instance :after ((self ~a-type) &key &allow-other-keys)
  (setf (%value-list self) '~a))

"              tyName (enum-list self))))
  
    
;; end mechanisms

(defun suffix-with-type (str-list)
  (mapcar #'(lambda (str)
	      (format nil "~a-type" str))
	  str-list))
	  
