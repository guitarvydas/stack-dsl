(in-package :stack-dsl)


(defparameter *pkg* "CL-USER")
(defparameter *slots-pkg* "CL-USER")

;;;
;; emission templates
;;;

(defmethod template-newscope ((s string-scanner) name)
  (pm-semit s "(defmethod $~a__NewScope ((p parser))~%" name)
  (pm-semit s "  (stack-dsl:%push-empty (~a::input-~a (env p))))~%~%" *slots-pkg* name))

(defmethod template-output ((s string-scanner) name)
  (pm-semit s "(defmethod $~a__Output ((p parser))~%" name)
  (pm-semit s "  (stack-dsl:%output (~a::output-~a (env p)) (~a::input-~a (env p)))~%" *slots-pkg* name *slots-pkg* name)
  (pm-semit s "  (stack-dsl:%pop (~a::input-~a (env p))))~%~%" *slots-pkg* name))

(defmethod template-coerce ((s string-scanner) to from)
  (pm-semit s "(defmethod $~a__CoerceFrom_~a ((p parser))~%" to from)
  (pm-semit s "  (let ((val (stack-dsl:%top (~a::output-~a (env p)))))~%" *slots-pkg* from)
  (pm-semit s "   (stack-dsl:%ensure-type \"~a\" val)~%" to)
  (pm-semit s "   (stack-dsl:%set-top (~a::input-~a (env p)) val)~%" *slots-pkg* to)
  (pm-semit s "   (stack-dsl:%pop (~a::output-~a (env p)))))~%~%" *slots-pkg* from))

(defmethod template-append ((s string-scanner) to from)
  (pm-semit s "(defmethod $~a__AppendFrom_~a ((p parser))~%" to from)
  (pm-semit s "  (let ((val (stack-dsl:%top (~a::output-~a (env p)))))~%" *slots-pkg* from) 
  (pm-semit s "    (let ((dest (stack-dsl:%top (~a::input-~a (env p)))))~%" *slots-pkg* to) 
  (pm-semit s "      (stack-dsl:%ensure-appendable-type dest)~%")
  (pm-semit s "      (stack-dsl:%ensure-type (stack-dsl:%element-type dest) val)~%")
  (pm-semit s "      (stack-dsl::%append dest val)~%")
  (pm-semit s "      (stack-dsl:%pop (~a::output-~a (env p))))))~%~%" *slots-pkg* from)) 

(defmethod template-set-field ((s string-scanner) to to-field from)
  (pm-semit s "(defmethod $~a__SetField_~a_from_~a ((p parser))~%" to to-field from)
  (pm-semit s "  (let ((val (stack-dsl:%top (~a::output-~a (env p)))))~%" *slots-pkg* from)
  (pm-semit s "    (stack-dsl:%ensure-field-type \"~a\" \"~a\" val)~%" to to-field)
  (pm-semit s "    (stack-dsl:%set-field (stack-dsl:%top (~a::input-~a (env p))) \"~a\" val)~%" *slots-pkg* to to-field)
  (pm-semit s "    (stack-dsl:%pop (~a::output-~a (env p)))))~%~%" *slots-pkg* from))

(defmethod template-set-enum ((s string-scanner) to val)
  (pm-semit s "(defmethod $~a__SetEnum_~a ((p parser))~%" to val)
  (pm-semit s "  (setf (stack-dsl:%value (stack-dsl:%top (~a::input-~a (env p)))) \"~a\"))~%~%" *slots-pkg* to val))


;;;
;; parsing (aka pattern matching)
;;;

(defun m-exprdsl (string-to-scan &key (out *standard-output*) (pkg "CL-USER") (slots-pkg "CL-USER"))
  (setf *pkg* pkg)
  (setf *slots-pkg* slots-pkg)
  (let ((s (make-instance 'string-scanner :text string-to-scan :out out)))
    (pm-semit s "(in-package ~s)~%~%" *pkg*)
    (@:loop
      (@:exit-when (>= (start s) (1- (length (text s)))))
      (when (m-id-look s)
	;; TODO: this will fail if "" at end of file...
        (m-exprdslparser s)))))

(defmethod m-exprdslparser ((s string-scanner))
    (let ((i (id s)))
      (template-newscope s i)
      (template-output s i)
      (pm-input s "\\=")
      (let ((r (cond ((pm-look s "\\{") (m-classWithFields s i))
		     ((pm-look s ":") (m-builtinType s i))
		     ((pm-look s "'") (m-enumList s i))
		     ((pm-look s "\\|") (m-compoundTypeList s i)))))
	(list i r))))

(defmethod m-classWithFields ((s string-scanner) class-name)
  (pm-input s "\\{")
  (let ((fields (m-idList s)))
    (pm-input s "\\}")
    (dolist (f fields)
      (template-set-field s class-name f f))))

(defmethod m-builtinType ((s string-scanner) class-name)
  (pm-input s ":")
  (cond ((pm-match s "map")    
	 (let ((element-name (id s)))
	   (template-append s class-name element-name)))
	((pm-match s "bag")  
	 (let ((element-type (id s)))
	   (template-append s class-name element-type)))
	((pm-match s "string")        "string")))

(defmethod m-enumList ((s string-scanner) class-name)
  (let ((c (m-enumConstant s)))
    (let ((vals (cons c (m-enumTail s))))
      (dolist (v vals)
	(template-set-enum s class-name v)))))

(defmethod m-enumTail ((s string-scanner))
  (cond ((pm-match s "\\|")
	 (let ((c (m-enumConstant s)))
	   (cons c (m-enumTail s))))
	(t nil)))

(defmethod m-compoundTypeList ((s string-scanner) class-name)
  (pm-input s "\\|")
  (let ((tyid (id s)))
    (let ((typelist (cons tyid (m-compoundTypeTail s))))
      (dolist (ty typelist)
	(template-coerce s class-name ty)))))

(defmethod m-compoundTypeTail ((s string-scanner))
  (cond ((pm-match s "\\|") 
         (let ((ty (id s)))
           (cons ty (m-compoundTypeTail s))))
	(t nil)))

(defmethod m-enumConstant ((s string-scanner))
  (pm-input s"'")
  (prog1
      (id s)
    (pm-input s "'")))

(defmethod m-idList ((s string-scanner))
  (cond ((pm-look s "\\w") (let ((id (id s))) (cons id (m-idList s))))
	(t nil)))

(defmethod m-id ((s string-scanner))
  (pm-input s "\\w+"))

(defmethod m-id-look ((s string-scanner))
  (pm-look s "\\w+"))

(defmethod m-ws ((s string-scanner))
  (pm-match s "[ \\t\\n\\r]+")
  nil)


;;;
;; alpha tests
;;;

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
