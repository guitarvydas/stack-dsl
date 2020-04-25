(in-package :stack-dsl)

;; none of these low-level operations check the type -
;; checking must be done by the caller

(defun %output (input-stack output-stack)
  ;; "return" the top item on the input-stack by pushing it onto the output
  ;; stack
  ;; no need to type-check, since DSL guarantees that the input and output types
  ;; are the same
  (let ((v (first input-stack)))
    (push v output-stack)))

(defun %push-empty (stack)
  ;; push some sort of empty indicator onto the stack
  (push :unbound stack))

(defun %replace-top (stack other-stack)
  ;; assign other to top of stack (no push), don't pop either stack
  (let ((v (first other-stack)))
    (pop stack)  ;; pop-push is replacement
    (push v stack)))

(defun %pop (stack)
  (pop stack))

(defun %top (stack)
  (first stack))

(defun %type-check-failure (expected val)
  (error (format nil "~%exected type ~a, but got ~a~%" expected val)))

(defun %ensure-appendable-type (stack)
  (if (eq '%bag (%type stack))
      :ok
      (if (eq '%map (%type stack))
	  :ok
	  (%type-check-failure "appendable" stack)))	  
  ;; fall-through => type checks out
  t) ;; don't care about return value

(defun %ensure-type (item expected-type)
  (if (eq (%type item) expected-type)
      :ok
      (%type-check-failure expected-type item))
  ;; fall-through => ok
  t) ;; don't care about return value

(defun %get-field (obj field-name)
  ;; return obj.field
  (slot-value field-name obj))

(defun %set-field (obj field-name val)
  ;; obj.field := val
  (setf (slot-value field-name obj) val))

(defmethod %append ((self %bag) (new-val %typed-value))
  (setf (lis self) (append (lis self) (list new-val))))

(defmethod %append ((self %map) (new-val %typed-value))
  (setf (lis self) (append (lis self) (list new-val))))
