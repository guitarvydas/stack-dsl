(in-package :stack-dsl)

(defclass stack-dsl-parser (pasm:parser)
  ((savedSymbol :accessor savedSymbol)
   (or-list :accessor or-list)
   (field-list :accessor field-list)
   (existence-list :accessor existence-list)))

(defmethod initially ((self stack-dsl-parser) token-list)
  (setf (savedSymbol self) nil)
  (setf (or-list self) nil)
  (setf (field-list self) nil)
  (setf (existence-list self) nil)
  (call-next-method))
  

  
