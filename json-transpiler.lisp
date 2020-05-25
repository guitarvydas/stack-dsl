(in-package :stack-dsl)

(defun json-transpile (file-name)
  (with-open-file (f file-name :direction :input)
    (let ((line nil))
      (read-line f nil :EOF)
      (@:loop
	(@:exit-when (eq :EOF line))
	(cond ((register-groups-bind (regs) (pattern-string string)
				     ...))
	      ((register-groups-bind 

		 (register-groups-bind (fname lname (#'parse-integer date month year))
      ("(\\w+)\\s+(\\w+)\\s+(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})" "Frank Zappa 21.12.1940")
    (list fname lname (encode-universal-time 0 0 0 date month year 0)))
("Frank" "Zappa" 1292889600)
