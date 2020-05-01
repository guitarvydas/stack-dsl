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
  
