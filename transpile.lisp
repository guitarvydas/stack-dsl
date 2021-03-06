(in-package :stack-dsl)

;(declaim (ftype (function (pasm:parser) t) stack-language))

(defun transpile-stack ( infile-name 
			target-package-name1 outfile-name
			jsonfile-name
			target-package-name2 target-package-name3 mechanisms-filename)
  ;; transpile a stack description in infile
  ;; to an output file of .lisp
(format *standard-output* "~&in stack-dsl ~a ~a~%" pasm:*pasm-tracing* pasm:*pasm-accept-tracing*)  
  (let ((in-string (alexandria:read-file-into-string infile-name)))
    (let ((tokens (scanner:scanner in-string)))
      (let ((p (make-instance 'stack-dsl-parser)))
	(initially p tokens)
	(setf (target-package p) target-package-name1)
	(stack-language p)
	(let ((result-string (get-output-stream-string (pasm:output-string-stream p))))
	  (with-open-file (f outfile-name :direction :output :if-exists :supersede :if-does-not-exist :create)
			  (write-string "(proclaim '(optimize (debug 3) (safety 3) (speed 0)))" f)
			  (write-string result-string f))
	  (format nil "file ~a written" outfile-name)
	  (with-open-file (jf jsonfile-name :direction :output :if-exists :supersede :if-does-not-exist :create)
	    (exprdsl in-string :out jf)
	    (format nil "file ~a written" jsonfile-name))
	  (with-open-file (m mechanisms-filename :direction :output :if-exists :supersede :if-does-not-exist :create)
	    (write-string "(proclaim '(optimize (debug 3) (safety 3) (speed 0)))" m)
	    (m-exprdsl in-string :out m :pkg target-package-name2 :slots-pkg target-package-name3)
	    (format nil "file ~a written" mechanisms-filename))
	  )))))

