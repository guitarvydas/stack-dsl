(in-package :stack-dsl)

(defun json-transpile (file-name)
  (with-open-file (f file-name :direction :input)
    (let ((line nil))
      (read-line f nil :EOF)
      (@:loop
	(@:exit-when (eq :EOF line))
	(cond ((
