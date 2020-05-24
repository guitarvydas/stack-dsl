(proclaim '(optimize (debug 3) (safety 3) (speed 0)))

(defclass string-scanner ()
  ((text :accessor text :initarg :text)
   (start :accessor start :initform 0)))

(defun exprdsl (string-to-scan)
  (let ((s (make-instance 'string-scanner :text string-to-scan)))
    (@:loop
      (@:exit-when 

    (let ((i (id s)))
      (input s "=")
      (let ((r (cond ((look s "{") (classWithFields s))
		     ((look s ":") (builtinType s))
		     ((look s "|") (compositeTypeList s)))))
	(list i r)))))

(defmethod classWithFields ((s string-scanner))
  (input s "{")
  (prog1 
      (idList s)
    (input s "}")))

(defmethod builtinType ((s string-scanner))
  (input s ":")
  (cond ((match s "map")    "map")
	((match s "bag")    "bag")
	((match s "string") "string")))

(defmethod enumList ((s string-scanner))
  (cond ((match "|" s)
	 (let ((c (enumConstant s)))
	   (cons c (enumList s))))
	(t nil)))

(defmethod compositeTypeList ((s string-scanner))
  (cond ((id? s) (let ((ty (id s)))
		   (cons ty (compositeTypeList s))))
	(t nil)))

(defmethod enumConstant ((s string-scanner))
  (input s"'")
  (prog1
      (id s)
    (input s "'")))

(defmethod idList ((s string-scanner))
  (cond ((look s "\\w") (let ((id (id s))) (cons id (idList s))))
	(t nil)))

(defmethod id ((s string-scanner))
  (prog2
      (ws s)
      (input s "\\w+")
    (ws s)))

(defmethod id? ((s string-scanner))
  (look s "\\w"))

(defmethod ws ((s string-scanner))
  (input s "[ \\t\\n\\r]*")
  nil)




(defmethod input ((s string-scanner) pattern-string)
  (multiple-value-bind (match-start match-end reg-start reg-ends)
      (cl-ppcre:scan (concatenate 'string "^" pattern-string)  
		     (text s)
		     :start (start s))
    (declare (ignore reg-start reg-ends))
    (if match-start
	(setf (start s) match-end)
	(error (format nil "parse error expecting ~s but got ~s" pattern-string (subseq (text s) (start s) (+ 10 (start s))))))))

(defmethod look ((s string-scanner) pattern-string)
  (multiple-value-bind (match-start match-end reg-start reg-ends)
      (cl-ppcre:scan (format nil "^(?=~a)" pattern-string)
		     (text s)
		     :start (start s))
    (declare (ignore match-end reg-start reg-ends))
    match-start))

(defmethod match ((s string-scanner) pattern)
  (and (look s pattern) (input s pattern)))


(defun test ()
  (let ((str "
expression = { ekind object }
ekind = 'true' | 'false' | 'object'
object = { name fieldMap }
fieldMap = :map field
field = { name parameterList }
parameterList =| nameMap
nameMap = :map name
name = :string
"
	  ))
    (exprdsl str)))
	  
