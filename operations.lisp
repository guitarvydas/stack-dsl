(in-package :stack-dsl)

;; none of these low-level operations check the type -
;; checking must be done by the caller

(defun %output (input-stack output-stack)
  ;; "return" the top item on the input-stack by pushing it onto the output
  ;; stack
  ;; no need to type-check, since DSL guarantees that the input and output types
  ;; are the same
  (assert (subtypep (type-of input-stack) '%typed-stack))
  (assert (subtypep (type-of output-stack) '%typed-stack))
  (let ((v (first (%stack input-stack))))
    (push v (%stack output-stack))))

(defun %push-empty (stack)
  ;; push some sort of empty indicator onto the stack
  (assert (subtypep (type-of stack) '%typed-stack))
  (let ((eltype (%element-type stack)))
    (let ((obj (make-instance eltype)))
      (setf (val obj) :unbound)
      (push obj (%stack stack)))))

(defun %replace-top (stack v)
  ;; assign other to top of stack (no push)
  (assert (subtypep (type-of stack) '%typed-stack))
  (pop (%stack stack))  ;; pop-push is replacement
  (push v (%stack stack)))

(defun %pop (stack)
  (assert (subtypep (type-of stack) '%typed-stack))
  (pop (%stack stack)))

(defun %top (stack)
  (assert (subtypep (type-of stack) '%typed-stack))
  (first (%stack stack)))

(defun %type-check-failure (expected val)
  (error (format nil "~%exected type ~a, but got ~a~%" expected val)))

(defun %ensure-appendable-type (stack)
  (assert (subtypep (type-of stack) '%typed-stack))
  (if (eq '%bag (type-of stack))
      :ok
      (if (eq '%map (type-of stack))
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
