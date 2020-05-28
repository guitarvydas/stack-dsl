(in-package :stack-dsl)


;; code templates
(defparameter *pkg* "CL-USER")
(defparameter *slots-pkg* "CL-USER")

(defmethod template-newscope ((s string-scanner) name)
  (semit s "(defmethod $~a__NewScope ((p parser))~%" name)
  (semit s "  (stack-dsl:%push-empty (~a::input-~a (env p))))~%~%" *slots-pkg* name))

(defmethod template-output ((s string-scanner) name)
  (semit s "(defmethod $~a__Output ((p parser))~%" name)
  (semit s "  (stack-dsl:%output (~a::output-~a (env p)) (~a::input-~a (env p)))~%" *slots-pkg* name *slots-pkg* name)
  (semit s "  (stack-dsl:%pop (~a::input-~a (env p))))~%~%" *slots-pkg* name))

(defmethod template-coerce ((s string-scanner) to from)
  (semit s "(defmethod $~a__CoerceFrom_~a ((p parser))~%" to from)
  (semit s "  (let ((val (stack-dsl:%top (~a::output-~a (env p)))))~%" *slots-pkg* from)
  (semit s "   (stack-dsl:%ensure-type \"~a\" val)~%" to)
  (semit s "   (stack-dsl:%push (~a::input-~a (env p)) val)~%" *slots-pkg* to)
  (semit s "   (stack-dsl:%pop (~a::output-~a (env p)))))~%~%" *slots-pkg* from))

(defmethod template-append ((s string-scanner) to from)
  (semit s "(defmethod $~a__AppendFrom_~a ((p parser))~%" to from)
  (semit s "  (let ((val (stack-dsl:%top (~a::output-~a (env p)))))~%" *slots-pkg* from) 
  (semit s "    (let ((dest (stack-dsl:%top (~a::input-~a (env p)))))~%" *slots-pkg* to) 
  (semit s "      (stack-dsl:%ensure-appendable-type dest)~%")
  (semit s "      (stack-dsl:%ensure-type (stack-dsl:%element-type dest) val)~%")
  (semit s "      (stack-dsl::%append dest val)~%")
  (semit s "      (stack-dsl:%pop (~a::output-~a (env p))))))~%~%" *slots-pkg* from)) 

(defmethod template-set-field ((s string-scanner) to to-field from)
  (semit s "(defmethod $~a__setField_~a_from_~a ((p parser))~%" to to-field from)
  (semit s "  (let ((val (stack-dsl:%top (~a::output-~a (env p)))))~%" *slots-pkg* from)
  (semit s "    (stack-dsl:%ensure-field-type \"~a\" \"~a\" val)~%" to to-field)
  (semit s "    (stack-dsl:%set-field (stack-dsl:%top (~a::input-~a (env p))) \"~a\" val)~%" *slots-pkg* to to-field)
  (semit s "    (stack-dsl:%pop (~a::output-~a (env p)))))~%~%" *slots-pkg* from))

(defmethod template-set-enum ((s string-scanner) to val)
  (semit s "(defmethod $~a__SetEnum_~a ((p parser))~%" to val)
  (semit s "  (setf (stack-dsl:%value (stack-dsl:%top (~a::input-~a (env p)))) \"~a\"))~%~%" *slots-pkg* to val))



(defun m-exprdsl (string-to-scan &key (out *standard-output*) (pkg "CL-USER") (slots-pkg "CL-USER"))
  (setf *pkg* pkg)
  (setf *slots-pkg* slots-pkg)
  (let ((s (make-instance 'string-scanner :text string-to-scan :out out)))
    (semit s "(in-package ~s)~%~%" *pkg*)
    (@:loop
      (@:exit-when (>= (start s) (1- (length (text s)))))
      (m-exprdslparser s))))

(defmethod m-exprdslparser ((s string-scanner))
    (let ((i (id s)))
      (template-newscope s i)
      (template-output s i)
      (input s "\\=")
      (let ((r (cond ((look s "\\{") (m-classWithFields s i))
		     ((look s ":") (m-builtinType s i))
		     ((look s "'") (m-enumList s i))
		     ((look s "\\|") (m-compoundTypeList s i)))))
	(list i r))))

(defmethod m-classWithFields ((s string-scanner) class-name)
  (input s "\\{")
  (let ((fields (m-idList s)))
    (input s "\\}")
    (dolist (f fields)
      (template-set-field s class-name f f))))

(defmethod m-builtinType ((s string-scanner) class-name)
  (input s ":")
  (cond ((match s "map")    
	 (let ((element-name (id s)))
	   (template-append s class-name element-name)))
	((match s "bag")  
	 (let ((element-type (id s)))
	   (template-append s class-name element-type)))
	((match s "string")        "string")))

(defmethod m-enumList ((s string-scanner) class-name)
  (let ((c (m-enumConstant s)))
    (let ((vals (cons c (m-enumTail s))))
      (dolist (v vals)
	(template-set-enum s class-name v)))))

(defmethod m-enumTail ((s string-scanner))
  (cond ((match s "\\|")
	 (let ((c (m-enumConstant s)))
	   (cons c (m-enumTail s))))
	(t nil)))

(defmethod m-compoundTypeList ((s string-scanner) class-name)
  (input s "\\|")
  (let ((tyid (id s)))
    (let ((typelist (cons tyid (m-compoundTypeTail s))))
      (dolist (ty typelist)
	(template-coerce s class-name ty)))))

(defmethod m-compoundTypeTail ((s string-scanner))
  (cond ((match s "\\|") 
         (let ((ty (id s)))
           (cons ty (m-compoundTypeTail s))))
	(t nil)))

(defmethod m-enumConstant ((s string-scanner))
  (input s"'")
  (prog1
      (id s)
    (input s "'")))

(defmethod m-idList ((s string-scanner))
  (cond ((look s "\\w") (let ((id (id s))) (cons id (m-idList s))))
	(t nil)))

(defmethod m-id ((s string-scanner))
  (input s "\\w+"))

(defmethod m-ws ((s string-scanner))
  (match s "[ \\t\\n\\r]+")
  nil)




(defun m-test ()
  (let ((str "
expression = { ekind object }
ekind = 'true' | 'false' | 'object'
object = { name fieldMap }
fieldMap = :map field
field = { name parameterList }
parameterList =| nameMap
nameMap = :map name
junk = :bag name
name = :string
"
	  ))
    (m-exprdsl str)))
	  
(defun m-test1 ()
  (let ((str "  "))
    (let ((s (make-instance 'string-scanner :text str)))
      (ws s))))
	  
(defun m-test2 ()
  (let ((str "abc"))
    (let ((s (make-instance 'string-scanner :text str)))
      (id s))))
	  
(defun m-test3 ()
  (let ((str "   abc"))
    (let ((s (make-instance 'string-scanner :text str)))
      (id s)
      s)))
	  
(defun m-test4 ()
  (let ((str "expression={ekind object}"))
    (let ((s (make-instance 'string-scanner :text str)))
      (m-exprdslparser s))))
	  
(defun m-test4a ()
  (let ((str "expression = { ekind object }"))
    (let ((s (make-instance 'string-scanner :text str)))
      (m-exprdslparser s))))
	  
(defun m-test5 ()
  (let ((str "ekind = 'true' | 'false' | 'object'"))
    (let ((s (make-instance 'string-scanner :text str)))
      (m-exprdslparser s))))
	  
(defun m-test6 ()
  (let ((str "parameterList =| a | b"))
    (let ((s (make-instance 'string-scanner :text str)))
      (m-exprdslparser s))))
	  
(defun m-test7 ()
  (let ((str "expression = :string"))
    (let ((s (make-instance 'string-scanner :text str)))
      (m-exprdslparser s))))
	  
(defun m-test8 ()
  (let ((str "expression = :bag x"))
    (let ((s (make-instance 'string-scanner :text str)))
      (m-exprdslparser s))))
	  
(defun m-test9 ()
  (let ((str "expression = :map y"))
    (let ((s (make-instance 'string-scanner :text str)))
      (m-exprdslparser s))))
	  
(defun m-test10 ()
  (let ((str "expression = { ekind object }
ekind = 'true' | 'false' | 'object'"))
    (m-exprdsl str)))
	  
(defun m-test11 ()
  (let ((str
         "
fieldMap = :map a
b = { name parameterList }
"))
    (m-exprdsl str)))

(defun m-testall ()
  (format *standard-output* "~&test1~%")
  (m-test1)
  (format *standard-output* "~&test2~%")
  (m-test2)
  (format *standard-output* "~&test3~%")
  (m-test3)
  (format *standard-output* "~&test4~%")
  (m-test4)
  (format *standard-output* "~&test4a~%")
  (m-test4a)
  (format *standard-output* "~&test5~%")
  (m-test5)
  (format *standard-output* "~&test6~%")
  (m-test6)
  (format *standard-output* "~&test7~%")
  (m-test7)
  (format *standard-output* "~&test8~%")
  (m-test8)
  (format *standard-output* "~&test9~%")
  (m-test9)
  (format *standard-output* "~&test10~%")
  (m-test10)
  (format *standard-output* "~&test11~%")
  (m-test11)
)


#| raw scanner, no emission
(defun m-exprdsl (string-to-scan &key (out *standard-output*))
  (let ((s (make-instance 'string-scanner :text string-to-scan :out out)))
    (@:loop
      (@:exit-when (>= (start s) (1- (length (text s)))))
      (m-exprdslparser s))))

(defmethod exprdslparser ((s string-scanner))
    (let ((i (id s)))
      (input s "\\=")
      (let ((r (cond ((look s "\\{") (classWithFields s))
		     ((look s ":") (builtinType s))
		     ((look s "'") (enumList s))
		     ((look s "\\|") (compoundTypeList s)))))
	(list i r))))

(defmethod classWithFields ((s string-scanner))
  (input s "\\{")
  (prog1 
      (idList s)
    (input s "\\}")))

(defmethod builtinType ((s string-scanner))
  (input s ":")
  (cond ((match s "map")    (id s) "map")
	((match s "bag")    (id s) "bag")
	((match s "string")        "string")))

(defmethod enumList ((s string-scanner))
  (let ((c (enumConstant s)))
    (cons c (enumTail s))))

(defmethod enumTail ((s string-scanner))
  (cond ((match s "\\|")
	 (let ((c (enumConstant s)))
	   (cons c (enumTail s))))
	(t nil)))

(defmethod compoundTypeList ((s string-scanner))
  (input s "\\|")
  (let ((tyid (id s)))
    (cons tyid (compoundTypeTail s))))

(defmethod compoundTypeTail ((s string-scanner))
  (cond ((match s "\\|") 
         (let ((ty (id s)))
           (cons ty (compoundTypeTail s))))
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

(defmethod ws ((s string-scanner))
  (match s "[ \\t\\n\\r]+")
  nil)

|#
