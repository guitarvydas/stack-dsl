(in-package :stack-dsl)

(defclass existence-list ()
  ((lis :accessor lis :initform (make-hash-table :test 'equal))))

(defclass stack-dsl-parser (pasm:parser)
  ((savedSymbol :accessor savedSymbol)
   (compound-list :accessor compound-list)
   (enum-list :accessor enum-list)
   (field-list :accessor field-list)
   (existence-list :accessor existence-list)
   (target-package :accessor target-package :initform "STACK-DSL")))

(defmethod initially ((self stack-dsl-parser) token-list)
(format *standard-output* "~&*** calling initially on stack-dsl-parser~%")
  (setf (savedSymbol self) nil)
  (setf (compound-list self) nil)
  (setf (enum-list self) nil)
  (setf (field-list self) nil)
  (setf (existence-list self) (make-instance 'existence-list))
  (call-next-method))

(defmethod push-existence-new ((self existence-list) str)
  (setf (gethash str (lis self)) str))

(defmethod map-existence-list ((self existence-list) function-of-one-arg)
  (maphash #'(lambda (k v)
	       (declare (ignore v))
	       (funcall function-of-one-arg k))
	   (lis self)))
  

;; all items must be %typed-value or %typed-struct or %typed-stack or %bag or %map or %string
(defclass %typed-value ()
  ((%type :accessor %type :initform :no-type :initarg :%type)
   (%value   :accessor %value   :initform :no-value)))

(defclass %compound-type (%typed-value)
  ((%type-list :accessor %type-list :initform nil :initarg :%type-list)))
  
(defclass %typed-stack ()
  ((%element-type :accessor %element-type :initform :no-type :initarg :%element-type)
   (%stack :accessor %stack :initform nil)))

(defclass %bag (%typed-value)
  ((%element-type :accessor %element-type :initform :no-type :initarg :%element-type)
   (lis :accessor lis :initform nil)))

(defclass %map (%typed-value)
  ((%element-type :accessor %element-type :initform :no-type :initarg :%element-type)
   (%ordered-list :accessor %ordered-list :initform nil)))

(defclass %string (%typed-value)
  ()
  (:default-initargs 
   :%type "STRING-TYPE"))

(defclass %null (%typed-value)
  ()
  (:default-initargs 
   :%type "NULL-TYPE"))

(defclass %enum (%typed-value)
  ((%value-list :accessor %value-list))
  (:default-initargs
   :%type 'enum))




(defclass type-descriptor () 
  ((descriptor-alist :accessor descriptor-alist :initarg :descriptor-alist)
   (%name :accessor %name :initform "")))

(defclass string-descriptor (type-descriptor)
  ())
(defclass null-descriptor (type-descriptor)
  ())
(defclass map-descriptor (type-descriptor)
  ((element-type :accessor element-type :initarg :element-type)))
(defclass bag-descriptor (type-descriptor)
  ((element-type :accessor element-type :initarg :element-type)))
(defclass enum-descriptor (type-descriptor)
  ((value-list :accessor value-list :initform nil :initarg :value-list)))
(defclass compound-descriptor (type-descriptor)
  ((types :accessor types :initform nil :initarg :types)))
(defclass structure-descriptor (type-descriptor)
  ((fields :accessor fields :initform nil :initarg :fields)))

