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
		     :types (make-types types-list)))))

(defun create-structure-descriptor (adesc)
  (let ((alist-name-type-pairs (cdr (assoc :fields adesc))))
    ;; (( (:FIELD-NAME . "exprkind") (:FIELD-TYPE . "exprkind") )...)
    (make-instance 'structure-descriptor
		   :descriptor-alist adesc
		   :fields (make-named-fields alist-name-type-pairs))))
  
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
  (same-enum-type-p self obj))
(defmethod deep-type-equal ((self enum-descriptor) (val STRING))
  (valid-enum-value-p self val))
(defmethod deep-type-equal ((self compound-descriptor) (obj compound-descriptor))
  (same-types-p self obj))
(defmethod deep-type-equal ((self structure-descriptor) (obj structure-descriptor))
  (same-fields-p self obj))

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
  (dolist (p pairs) ;; pairs is a list of named-field (derived from ((:field-name . "exprKind") (:field-type . "exprKind")))
      (when (string= field-name (name p))
	(return-from find-field-type (ty p))))
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

(defmethod enum-value ((self %enum))
  (%value self))

(defmethod valid-enum-value-p ((self enum-descriptor) (val STRING))
  (let ((strlist (value-list self)))
    (dolist (s strlist)
      (when (string= s val)
	(return-from valid-enum-value-p t))))
  nil)

(defmethod valid-type-p ((self compound-descriptor) (other-type STRING))
    (let ((self-types (types self)))
      (some #'(lambda (x) (string= x other-type)) self-types)))
  
(defmethod same-types-p ((self compound-descriptor) (obj compound-descriptor))
  (let ((self-types (types self))
	(obj-types (types obj)))
    (and (= (length self-types) (length obj-types))
	 (every #'(lambda (ty) (valid-type-p self ty)) obj-types))))

  
(defmethod valid-type-p ((self named-field) (other named-field))
  (string= (ty self) (ty other)))

(defmethod valid-type-p ((self structure-descriptor) (other named-field))
  (some #'(lambda (sf) (valid-type-p sf other)) (fields self)))

(defmethod valid-field-p ((self structure-descriptor) (other-field STRING))
    (let ((self-types (fields self)))
      (some #'(lambda (x) (string= x other-type)) self-types)))
  
(defmethod same-fields-p ((self structure-descriptor) (obj structure-descriptor))
  (let ((self-fields (fields self))
	(obj-fields (fields obj)))
    (and (= (length self-fields) (length obj-fields))
	 (every #'(lambda (f) (valid-type-p self f)) obj-fields)))) ;; TODO: what if typeappears more than once?  Not an issue for current DSL (since types appear 0 or 1 times, since name and type are the same).
  
(defun make-named-fields (alist)
  (let ((result nil))
    (dolist (pair alist)
      (let ((f (make-instance 'named-field 
			      :name (cdr (assoc :field-name pair))
			      :ty   (cdr (assoc :field-type pair)))))
	(push f result)))
    result))
  
(defun make-types (string-list)
  string-list)

(defun same-enum-type-p (self other)
  (or (string= (%name self) (%name other))
      (every-enumerated-value-same-p (value-list self) (value-list other))))

(defun every-enumerated-value-same-p (self-values other-values)
  (cond ((null self-values)
	 (null other-values))
	(t (and (string= (car self-values) (car other-values))
		(every-enumerated-value-same-p (cdr self-value) (cdr other-values))))))
