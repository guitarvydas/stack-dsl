(in-package :stack-dsl)

;; internal support for stack language
(defclass %checked-type ()
  ((%type :accessor %type :initform nil)))

(defmethod %check-type ((self %checked-type) item)
  (unless (eq (%type self) (type-of item))
    (error (format nil "type ~a does not match ~a" (type-of item) (%type self)))))

(defclass %typed-stack (%checked-type)
  ((%stack :accessor %stack :initform nil)))

(defmethod %push ((self %typed-stack) item)
  (%check-type self item)
  (cl:push item (%stack self)))

(defmethod %pop ((self %typed-stack))
  (when (null (%stack self))
    (error "stack empty for ~a" self))
  (cl:pop (%stack self)))

(defmethod %get ((self %typed-stack))
  (first (%stack self)))

(defun check-field-type (self field-name)
  (unless (cl:slot-exists-p (first (%stack self)) field-name)
    (error (format nil "field name ~a is invalid for ~a" field-name (%type self)))))

(defmethod %get-field ((self %typed-stack) field-name)
  (when (null (%stack self))
    (error "stack empty for ~a" self))
  (check-field-type self field-name)
  (slot-value field-name (first (%stack self))))

(defun check-that-data-type-matches-destination-field (self field-name data)
  ;; tbd
  (declare (ignore self field-name data))
  )

(defmethod %set-field ((self %typed-stack) field-name data)
  (when (null (%stack self))
    (error "stack empty for ~a" self))
  (check-field-type self field-name)
  (check-that-data-type-matches-destination-field self field-name data)
  (setf (slot-value field-name (first (%stack self)))
	data))

(defclass %typed-bag ()
  ((%type :accessor %type :initform 'bag)
   (%bag :accessor %bag :initform nil)))

(defun %check-existence (class-symbol)
  (let ((c (find-class class-symbol)))
    (when (null c)
      (error "class ~a has not been defined" class-symbol)))
  T)
