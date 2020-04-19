(in-package :stack-dsl)

(defun test ()
  (let ((p (make-instance 'stack-dsl-parser)))
    (let ((pasm:*pasm-tracing* t))
      (let ((r (pasm:transpile p *pasm* *use-dsl* 'stack-language)))
	r))))
