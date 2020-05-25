(in-package :stack-dsl)

(defclass string-scanner ()
  ((text :accessor text :initarg :text)
   (start :accessor start :initform 0)
   (previous-start :accessor previous-start :initform 0)
   (out  :accessor out :initarg :out :initform *standard-output*)))

(defmethod semit ((s string-scanner) format-string &rest format-args)
  (apply 'format (out s) format-string format-args))

(defmethod save-start ((s string-scanner))
  (setf (previous-start s) (start s)))

(defmethod get-accepted ((s string-scanner))
  (subseq (text s) (previous-start s) (start s)))


(defun exprdsl (string-to-scan &key (out *standard-output*))
  (let ((s (make-instance 'string-scanner :text string-to-scan :out out)))
    (semit s "[~%")
    (@:loop
      (exprdslparser s)
      (@:exit-when (>= (start s) (1- (length (text s)))))
      (semit s ",~%"))
    (semit s "~&]~%")))

(defmethod exprdslparser ((s string-scanner))
    (let ((i (id s)))
      (semit s "{ \"name\" : \"~a\" , " i)
      (semit s "\"descriptor\" : ")
      (input s "\\=")
      (let ((r (cond ((look s "\\{") (classWithFields s))
		     ((look s ":") (builtinType s))
		     ((look s "'") (enumList s))
		     ((look s "\\|") (compoundTypeList s)))))
	(semit s "}"))))
	

(defmethod classWithFields ((s string-scanner))
  (input s "\\{")
  (semit s "{ \"kind\" : \"structure\", \"fields\" : [")
  (let ((flist (fieldIdList s)))
    (input s "\\}")
    (@:loop
      (let ((f (pop flist)))
	(semit s "~&{\"fieldName\":\"~a\",\"fieldType\":\"~a\"}" f f))
      (@:exit-when (null flist))
      (semit s ",")))
  (semit s "]}"))

(defmethod builtinType ((s string-scanner))
  (input s ":")
  (semit s "{ \"kind\" : ")
  (cond ((match s "map") (let ((i (id s))) (semit s "\"map\", \"elementType\" : \"~a\"" i)))
	((match s "bag") (let ((i (id s))) (semit s "\"bag\", \"elementType\" : \"~a\"" i)))
	((match s "string")                (semit s "\"string\"")))
  (semit s " }"))

(defmethod enumList ((s string-scanner))
  (let ((elist (let ((c (enumConstant s)))
		 (cons c (enumTail s)))))
    (semit s "{ \"kind\" : \"enum\", \"valueList\" : [")
    (@:loop
      (let ((e (pop elist)))
	(semit s "\"~a\"" e))
      (@:exit-when (null elist))
      (semit s ","))
    (semit s "] }")))



(defmethod enumTail ((s string-scanner))
  (cond ((match s "\\|")
	 (let ((c (enumConstant s)))
	   (cons c (enumTail s))))
	(t nil)))

(defmethod enumConstant ((s string-scanner))
  (input s"'")
  (prog1
      (id s)
    (input s "'")))

(defmethod compoundTypeList ((s string-scanner))
  (input s "\\|")
  (semit s "{ \"kind\" : \"compound\", \"types\" : [")
  (let ((tylist
	 (let ((tyid (id s)))
	   (cons tyid (compoundTypeTail s)))))
    (@:loop
      (let ((ty (pop tylist)))
	(semit s "\"~a\"" ty)
	(@:exit-when (null tylist))
	(semit s ",")))
    (semit s "] }")))

(defmethod compoundTypeTail ((s string-scanner))
  (cond ((match s "\\|") 
         (let ((ty (id s)))
           (cons ty (compoundTypeTail s))))
	(t nil)))

(defmethod idList ((s string-scanner))
  (cond ((look s "\\w") (let ((i (id s))) (cons i (idList s))))
	(t nil)))

(defmethod fieldIdList ((s string-scanner))
  (idList s))

(defmethod id ((s string-scanner))
  (input s "\\w+")
  (get-accepted s))

(defmethod ws ((s string-scanner))
  (match s "[ \\t\\n\\r]+")
  nil)


(defmethod skip ((s string-scanner))
  (save-start s)
  (multiple-value-bind (match-start match-end reg-start reg-ends)
      (cl-ppcre:scan "^[ \\t\\n\\r]+"
		     (text s)
		     :start (start s))
    (declare (ignore reg-start reg-ends))
    (when (and match-start (> match-end (start s)))
	(setf (start s) match-end)))
  (save-start s))

(defmethod input ((s string-scanner) pattern-string)
  (skip s)
  (multiple-value-bind (match-start match-end reg-start reg-ends)
      (cl-ppcre:scan (concatenate 'string "^" pattern-string)  
		     (text s)
		     :start (start s))
    (declare (ignore reg-start reg-ends))
    (if (and match-start (> match-end (start s)))
	(setf (start s) match-end)
	(error (format nil "parse error expecting ~s but got ~s" pattern-string (subseq (text s) (start s) (min (length (text s)) (+ 10 (start s)))))))
    t))

(defmethod look ((s string-scanner) pattern-string)
  (skip s)
  (multiple-value-bind (match-start match-end reg-start reg-ends)
      (cl-ppcre:scan (format nil "^(?=~a)" pattern-string)
		     (text s)
		     :start (start s))
    (declare (ignore match-end reg-start reg-ends))
    match-start))

(defmethod match ((s string-scanner) pattern)
  (skip s)
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
