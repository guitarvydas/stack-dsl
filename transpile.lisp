(in-package :stack-dsl)

;(declaim (ftype (function (pasm:parser) t) stack-language))

(defun transpile-stack (target-package-name infile-name outfile-name)
  ;; transpile a stack description in infile
  ;; to an output file of .lisp
(format *standard-output* "~&in stack-dsl~%")  
  (let ((in-string (alexandria:read-file-into-string infile-name)))
    (let ((tokens (scanner:scanner in-string)))
      (let ((p (make-instance 'stack-dsl-parser)))
	(initially p tokens)
	(setf (target-package p) target-package-name)
	(stack-language p)
	(let ((result-string (get-output-stream-string (pasm:output-string-stream p))))
	  (with-open-file (f outfile-name :direction :output :if-exists :supersede :if-does-not-exist :create)
			  (write-string result-string f))
	  (format nil "file ~a written" outfile-name))))))

