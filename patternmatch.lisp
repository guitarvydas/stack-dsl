(in-package :stack-dsl)

(defmethod id ((s string-scanner))
  (pm-input s "\\w+")
  (pm-get-accepted s))



(defun exprdsl (string-to-scan &key (out *standard-output*))
  (let ((s (make-instance 'string-scanner :text string-to-scan :out out)))
    (pm-semit s "[~%")
    (@:loop
      (exprdslparser s)
      (@:exit-when (>= (start s) (1- (length (text s)))))
      (pm-semit s ",~%"))
    (pm-semit s "~&]~%")))

(defmethod exprdslparser ((s string-scanner))
    (let ((i (id s)))
      (pm-semit s "{ \"name\" : \"~a\" , " i)
      (pm-semit s "\"descriptor\" : ")
      (pm-input s "\\=")
      (cond ((pm-look s "\\{") (classWithFields s))
	    ((pm-look s ":") (builtinType s))
	    ((pm-look s "'") (enumList s))
	    ((pm-look s "\\|") (compoundTypeList s)))
	(pm-semit s "}")))
	

(defmethod classWithFields ((s string-scanner))
  (pm-input s "\\{")
  (pm-semit s "{ \"kind\" : \"structure\", \"fields\" : [")
  (let ((flist (fieldIdList s)))
    (pm-input s "\\}")
    (@:loop
      (let ((f (pop flist)))
	(pm-semit s "~&{\"fieldName\":\"~a\",\"fieldType\":\"~a\"}" f f))
      (@:exit-when (null flist))
      (pm-semit s ",")))
  (pm-semit s "]}"))

(defmethod builtinType ((s string-scanner))
  (pm-input s ":")
  (pm-semit s "{ \"kind\" : ")
  (cond ((pm-match) s "map") (let ((i (id s))) (pm-semit s "\"map\", \"elementType\" : \"~a\"" i))
	((pm-match) s "bag") (let ((i (id s))) (pm-semit s "\"bag\", \"elementType\" : \"~a\"" i))
	((pm-match) s "string")                (pm-semit s "\"string\""))
  (pm-semit s " }"))

(defmethod enumList ((s string-scanner))
  (let ((elist (let ((c (enumConstant s)))
		 (cons c (enumTail s)))))
    (pm-semit s "{ \"kind\" : \"enum\", \"valueList\" : [")
    (@:loop
      (let ((e (pop elist)))
	(pm-semit s "\"~a\"" e))
      (@:exit-when (null elist))
      (pm-semit s ","))
    (pm-semit s "] }")))



(defmethod enumTail ((s string-scanner))
  (cond ((pm-match) s "\\|")
	 (let ((c (enumConstant s)))
	   (cons c (enumTail s))))
	(t nil))

(defmethod enumConstant ((s string-scanner))
  (pm-input s"'")
  (prog1
      (id s)
    (pm-input s "'")))

(defmethod compoundTypeList ((s string-scanner))
  (pm-input s "\\|")
  (pm-semit s "{ \"kind\" : \"compound\", \"types\" : [")
  (let ((tylist
	 (let ((tyid (id s)))
	   (cons tyid (compoundTypeTail s)))))
    (@:loop
      (let ((ty (pop tylist)))
	(pm-semit s "\"~a\"" ty)
	(@:exit-when (null tylist))
	(pm-semit s ",")))
    (pm-semit s "] }")))

(defmethod compoundTypeTail ((s string-scanner))
  (cond ((pm-match) s "\\|") 
         (let ((ty (id s)))
           (cons ty (compoundTypeTail s)))
	 (t nil)))

(defmethod idList ((s string-scanner))
  (cond ((pm-look s "\\w") (let ((i (id s))) (cons i (idList s))))
	(t nil)))

(defmethod fieldIdList ((s string-scanner))
  (idList s))

(defmethod ws ((s string-scanner))
  (pm-match s "[ \\t\\n\\r]+")
  nil)


(defun test ()
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
  (let ((str "expression={ekind object}"))
    (let ((s (make-instance 'string-scanner :text str)))
      (exprdslparser s))))
	  
(defun test4a ()
  (let ((str "expression = { ekind object }"))
    (let ((s (make-instance 'string-scanner :text str)))
      (exprdslparser s))))
	  
(defun test5 ()
  (let ((str "ekind = 'true' | 'false' | 'object'"))
    (let ((s (make-instance 'string-scanner :text str)))
      (exprdslparser s))))
	  
(defun test6 ()
  (let ((str "parameterList =| a | b"))
    (let ((s (make-instance 'string-scanner :text str)))
      (exprdslparser s))))
	  
(defun test7 ()
  (let ((str "expression = :string"))
    (let ((s (make-instance 'string-scanner :text str)))
      (exprdslparser s))))
	  
(defun test8 ()
  (let ((str "expression = :bag x"))
    (let ((s (make-instance 'string-scanner :text str)))
      (exprdslparser s))))
	  
(defun test9 ()
  (let ((str "expression = :map y"))
    (let ((s (make-instance 'string-scanner :text str)))
      (exprdslparser s))))
	  
(defun test10 ()
  (let ((str "expression = { ekind object }
ekind = 'true' | 'false' | 'object'"))
    (exprdsl str)))
	  
(defun test11 ()
  (let ((str
         "
fieldMap = :map a
b = { name parameterList }
"))
    (exprdsl str)))

(defun testall ()
  (format *standard-output* "~&test1~%")
  (test1)
  (format *standard-output* "~&test2~%")
  (test2)
  (format *standard-output* "~&test3~%")
  (test3)
  (format *standard-output* "~&test4~%")
  (test4)
  (format *standard-output* "~&test4a~%")
  (test4a)
  (format *standard-output* "~&test5~%")
  (test5)
  (format *standard-output* "~&test6~%")
  (test6)
  (format *standard-output* "~&test7~%")
  (test7)
  (format *standard-output* "~&test8~%")
  (test8)
  (format *standard-output* "~&test9~%")
  (test9)
  (format *standard-output* "~&test10~%")
  (test10)
  (format *standard-output* "~&test11~%")
  (test11)
)


#| raw scanner, no emission
(defun exprdsl (string-to-scan &key (out *standard-output*))
  (let ((s (make-instance 'string-scanner :text string-to-scan :out out)))
    (@:loop
      (@:exit-when (>= (start s) (1- (length (text s)))))
      (exprdslparser s))))

(defmethod exprdslparser ((s string-scanner))
    (let ((i (id s)))
      (pm-input s "\\=")
      (let ((r (cond ((pm-look s "\\{") (classWithFields s))
		     ((pm-look s ":") (builtinType s))
		     ((pm-look s "'") (enumList s))
		     ((pm-look s "\\|") (compoundTypeList s)))))
	(list i r))))

(defmethod classWithFields ((s string-scanner))
  (pm-input s "\\{")
  (prog1 
      (idList s)
    (pm-input s "\\}")))

(defmethod builtinType ((s string-scanner))
  (pm-input s ":")
  (cond ((pm-match) s "map")    (id s) "map")
	((pm-match) s "bag")    (id s) "bag")
	((pm-match) s "string")        "string")))

(defmethod enumList ((s string-scanner))
  (let ((c (enumConstant s)))
    (cons c (enumTail s))))

(defmethod enumTail ((s string-scanner))
  (cond ((pm-match) s "\\|")
	 (let ((c (enumConstant s)))
	   (cons c (enumTail s))))
	(t nil)))

(defmethod compoundTypeList ((s string-scanner))
  (pm-input s "\\|")
  (let ((tyid (id s)))
    (cons tyid (compoundTypeTail s))))

(defmethod compoundTypeTail ((s string-scanner))
  (cond ((pm-match) s "\\|") 
         (let ((ty (id s)))
           (cons ty (compoundTypeTail s))))
	(t nil)))

(defmethod enumConstant ((s string-scanner))
  (pm-input s"'")
  (prog1
      (id s)
    (pm-input s "'")))

(defmethod idList ((s string-scanner))
  (cond ((pm-look s "\\w") (let ((id (id s))) (cons id (idList s))))
	(t nil)))

(defmethod id ((s string-scanner))
  (pm-input s "\\w+"))

(defmethod ws ((s string-scanner))
  (pm-match) s "[ \\t\\n\\r]+")
  nil)

|#
