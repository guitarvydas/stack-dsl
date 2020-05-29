(in-package :stack-dsl)

(defmethod skip ((s string-scanner))
  (save-start s)
  (multiple-value-bind (match-start match-end reg-start reg-ends)
      (cl-ppcre:scan "^[ \\t\\n\\r]+"
		     (text s)
		     :start (start s))
    (declare (ignore reg-start reg-ends))
    (when (and match-start (> match-end (start s)))
	(setf (start s) match-end)))
  (save-start s))

(defmethod input ((s string-scanner) pattern-string)
  (skip s)
  (multiple-value-bind (match-start match-end reg-start reg-ends)
      (cl-ppcre:scan (concatenate 'string "^" pattern-string)  
		     (text s)
		     :start (start s))
    (declare (ignore reg-start reg-ends))
    (if (and match-start (> match-end (start s)))
	(setf (start s) match-end)
	(error (format nil "parse error expecting ~s but got ~s" pattern-string (subseq (text s) (start s) (min (length (text s)) (+ 10 (start s)))))))
    t))

(defmethod look ((s string-scanner) pattern-string)
  (skip s)
  (multiple-value-bind (match-start match-end reg-start reg-ends)
      (cl-ppcre:scan (format nil "^(?=~a)" pattern-string)
		     (text s)
		     :start (start s))
    (declare (ignore match-end reg-start reg-ends))
    match-start))

(defmethod match ((s string-scanner) pattern)
  (skip s)
  (and (look s pattern) (input s pattern) t))


