(in-package :stack-dsl)

(defparameter *pasm-script* "stack-dsl.pasm")
(defparameter *generated-lisp* "stack-dsl.lisp")


;; patterns...

;; id = { ... }     --> class with fields def
;; id = :bag ...    --> bag def
;; id = :string ... --> string def
;; id = :map ...    --> map def
;; id = | ...       --> or type def

(defun generate (&optional (given-filename nil))
  (let ((output-filename (or given-filename 
		     (asdf:system-relative-pathname :stack-dsl *generated-lisp*))))
    (pasm:pasm-to-file 'random-symbol-from-this-package
		       (asdf:system-relative-pathname :stack-dsl *pasm-script*)
		       output-filename)
    (format nil "generated ~a" output-filename)))
  


