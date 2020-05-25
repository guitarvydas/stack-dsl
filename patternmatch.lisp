(proclaim '(optimize (debug 3) (safety 3) (speed 0)))

(defclass string-scanner ()
  ((text :accessor text :initarg :text)
   (start :accessor start :initform 0)))

(defun exprdsl (string-to-scan)
  (let ((s (make-instance 'string-scanner :text string-to-scan)))
    (@:loop
      (format *standard-output* "~&start = ~a~%" (start s))
      (@:exit-when (= (length string-to-scan) (start s)))
      (exprdslparser s))))

(defmethod exprdslparser ((s string-scanner))
    (let ((i (id s)))
      (input s "=")
      (let ((r (cond ((look s "{") (classWithFields s))
		     ((look s ":") (builtinType s))
		     ((look s "'") (enumList s))
		     ((look s "\\|") (compositeTypeList s)))))
	(list i r))))

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
  (let ((c (enumConstant s)))
    (cons c (enumTail s))))

(defmethod enumTail ((s string-scanner))
  (cond ((match s "|")
	 (let ((c (enumConstant s)))
	   (cons c (enumTail s))))
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
  (input s "\\w+"))

(defmethod id? ((s string-scanner))
  (look s "\\w"))

(defmethod ws ((s string-scanner))
  (match s "[ \\t\\n\\r]+")
  nil)


(defmethod skip ((s string-scanner))
  (multiple-value-bind (match-start match-end reg-start reg-ends)
      (cl-ppcre:scan "^[ \\t\\n\\r]+"
		     (text s)
		     :start (start s))
    (declare (ignore reg-start reg-ends))
    (when match-start
	(setf (start s) match-end)
	(format *standard-output* "~a" (subseq (text s) match-start match-end)))))

(defmethod input ((s string-scanner) pattern-string)
  (skip s)
  (multiple-value-bind (match-start match-end reg-start reg-ends)
      (cl-ppcre:scan (concatenate 'string "^" pattern-string)  
		     (text s)
		     :start (start s))
    (declare (ignore reg-start reg-ends))
    (if match-start
	(setf (start s) match-end)
	(error (format nil "parse error expecting ~s but got ~s" pattern-string (subseq (text s) (start s) (min (length (text s)) (+ 10 (start s)))))))
(format *standard-output* "~a" (subseq (text s) match-start match-end))))

(defmethod look ((s string-scanner) pattern-string)
  (skip s)
  (multiple-value-bind (match-start match-end reg-start reg-ends)
      (cl-ppcre:scan (format nil "^(?=~a)" pattern-string)
		     (text s)
		     :start (start s))
    (declare (ignore match-end reg-start reg-ends))
    (and match-start (> match-start (start s)))))

(defmethod match ((s string-scanner) pattern)
  (skip s)
  (format *standard-output* "~&match ~a look(~s)=~a~%" (start s) pattern (look s pattern))
  (and (look s pattern) (input s pattern) t))


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
	  
(defun test1 ()
  (let ((str "  "))
    (let ((s (make-instance 'string-scanner :text str)))
      (ws s))))
	  
(defun test2 ()
  (let ((str "abc"))
    (let ((s (make-instance 'string-scanner :text str)))
      (id s))))
	  
(defun test3 ()
  (let ((str "   abc"))
    (let ((s (make-instance 'string-scanner :text str)))
      (id s)
      s)))
	  
(defun test4 ()
  (let ((str "expression = { ekind object }"))
    (let ((s (make-instance 'string-scanner :text str)))
      (exprdslparser s))))
	  
(defun test5 ()
  (let ((str "ekind = 'true' | 'false' | 'object'"))
    (let ((s (make-instance 'string-scanner :text str)))
      (exprdslparser s))))
	  
(defun test6 ()
  (let ((str "parameterList =| nameMap | nameMap"))
    (let ((s (make-instance 'string-scanner :text str)))
      (exprdslparser s))))
	  
(defun test7 ()
  (let ((str "expression = :string"))
    (let ((s (make-instance 'string-scanner :text str)))
      (exprdslparser s))))
	  
(defun test8 ()
  (let ((str "expression = :bag"))
    (let ((s (make-instance 'string-scanner :text str)))
      (exprdslparser s))))
	  
(defun test9 ()
  (let ((str "expression = :map"))
    (let ((s (make-instance 'string-scanner :text str)))
      (exprdslparser s))))
	  
(defun test10 ()
  (let ((str "expression = { ekind object }
ekind = 'true' | 'false' | 'object'"))
    (let ((s (make-instance 'string-scanner :text str)))
      (exprdslparser s))))
	  
