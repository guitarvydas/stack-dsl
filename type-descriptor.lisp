(in-package :stack-dsl)

;; Type descriptors allow us to examine / parse types at runtime.
;; Enum was added later, it looks like it might contain redundant info (e.g.
;;   the enum descriptor has a list of all enum values, and, the enum type itself
;;   carries a list of values - we can probably trim the list from the enum type - later
;;   see deep-type-equal, too)

(defmethod initialize-instance :after ((self type-descriptor) &key &allow-other-keys)
  (let ((kname (cdr (assoc :kind (descriptor-alist self)))))
    (when kname
      (setf (%name self) kname))))

(defun create-string-descriptor (adesc)
  (make-instance 'string-descriptor :descriptor-alist adesc))

(defun create-null-descriptor (adesc)
  (make-instance 'null-descriptor :descriptor-alist adesc))

(defun create-map-descriptor (adesc)
  (let ((eltype (intern (string-upcase (cdr (assoc :element-type adesc))) "KEYWORD")))
    (make-instance 'map-descriptor 
		   :descriptor-alist adesc
		   :element-type eltype)))
  
(defun create-bag-descriptor (adesc)
  (let ((eltype (intern (string-upcase (cdr (assoc :element-type adesc))) "KEYWORD")))
    (make-instance 'bag-descriptor 
		   :descriptor-alist adesc
		   :element-type eltype)))
  
(defun create-enum-descriptor (adesc)
  (let ((values-string-list (cdr (assoc :value-list adesc))))
    (make-instance 'enum-descriptor
		   :descriptor-alist adesc
		   :value-list values-string-list)))

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
		 (:null (create-null-descriptor adesc))
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
(defmethod deep-type-equal ((self null-descriptor) (obj null-descriptor))
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
(defmethod deep-type-equal ((self enum-descriptor) (val STRING))
  (let ((self-values (values self)))
    (string-member val self-values)))
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
(defmethod shallow-type-equal ((self-desc type-descriptor) (obj %typed-value))
  (string= (%name self-desc) (%type obj)))

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


(defun read-json-types (filename)
  (alexandria:read-file-into-string filename))

(defun types-as-alist (filename)
  (let ((str (read-json-types filename)))
    (with-input-from-string (s str)
      (json:decode-json s))))

(defun initialize-types (filename)
  (setf *type-table* (types-as-alist filename))
  (setf *type-hash* (make-hash-table :test 'equal))
  (initialize-type-hash *type-hash* *type-table*)
  (maphash #'(lambda (k v)
	       (format *standard-output* "~&type ~s ~s~%" k v))
	   *type-hash*))


(defmethod valid-enum-tag-p (not-enum val)
  (declare (ignore not-enum val))
  (error "valid-enum-tag-p called on non-enum"))

(defmethod valid-enum-tag-p ((self enum-descriptor) (s STRING))
  (some #'(lambda (val) (string-equal val s)) (value-list self)))


(defmethod enum-p ((self T))
  nil)

(defmethod enum-p ((self enum-descriptor))
  t)

