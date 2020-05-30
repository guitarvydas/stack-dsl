(in-package :stack-dsl)

(defclass string-scanner ()
  ((text :accessor text :initarg :text)
   (start :accessor start :initform 0)
   (previous-start :accessor previous-start :initform 0)
   (out  :accessor out :initarg :out :initform *standard-output*)))

(defmethod pm-semit ((s string-scanner) format-string &rest format-args)
  (apply 'format (out s) format-string format-args))

(defmethod pm-save-start ((s string-scanner))
  (setf (previous-start s) (start s)))

(defmethod pm-get-accepted ((s string-scanner))
  (subseq (text s) (previous-start s) (start s)))

(defmethod pm-skip ((s string-scanner))
  (multiple-value-bind (match-start match-end reg-start reg-ends)
      (cl-ppcre:scan "^([ \\t\\n\\r]+|%.*[\\n\\r])"
		     (text s)
		     :start (start s))
    (declare (ignore reg-start reg-ends))
    (when (and match-start (> match-end (start s)))
	(setf (start s) match-end)))
  (pm-save-start s))

(defmethod pm-input ((s string-scanner) pattern-string)
  (pm-skip s)
  (multiple-value-bind (match-start match-end reg-start reg-ends)
      (cl-ppcre:scan (concatenate 'string "^" pattern-string)  
		     (text s)
		     :start (start s))
    (declare (ignore reg-start reg-ends))
    (if (and match-start (> match-end (start s)))
	(setf (start s) match-end)
	(error (format nil "parse error expecting ~s but got ~s" pattern-string (subseq (text s) (start s) (min (length (text s)) (+ 10 (start s)))))))
    t))

(defmethod pm-look ((s string-scanner) pattern-string)
  (pm-skip s)
  (multiple-value-bind (match-start match-end reg-start reg-ends)
      (cl-ppcre:scan (format nil "^(?=~a)" pattern-string)
		     (text s)
		     :start (start s))
    (declare (ignore match-end reg-start reg-ends))
    match-start))

(defmethod pm-match ((s string-scanner) pattern)
  (pm-skip s)
  (and (pm-look s pattern) (pm-input s pattern) t))


