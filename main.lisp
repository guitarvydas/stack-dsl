(in-package :stack-dsl)

(assert nil)

(defparameter *pasm* (alexandria:read-file-into-string
		      (asdf:system-relative-pathname :stack-dsl "stack-dsl.pasm")))

(defparameter *use-dsl* (alexandria:read-file-into-string 
		      (asdf:system-relative-pathname :stack-dsl "stack.dsl")))


;; patterns...

;; id = { ... }     --> class with fields def
;; id = :bag ...    --> bag def
;; id = :string ... --> string def
;; id = :map ...    --> map def
;; id = | ...       --> or type def
