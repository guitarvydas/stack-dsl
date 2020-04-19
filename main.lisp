(in-package :stack-dsl)

(defparameter *pasm* (alexandria:read-file-into-string
		      (asdf:system-relative-pathname :stack-dsl "dsl.pasm")))

(defparameter *use-dsl* (alexandria:read-file-into-string 
		      (asdf:system-relative-pathname :stack-dsl "stack.dsl")))

(defparameter *internal-support* (alexandria:read-file-into-string 
				  (asdf:system-relative-pathname :stack-dsl "internal-support.lisp")))


;; patterns...

;; id = { ... }     --> class with fields def
;; id = :bag ...    --> bag def
;; id = :string ... --> string def
;; id = :map ...    --> map def
;; id = | ...       --> or type def
