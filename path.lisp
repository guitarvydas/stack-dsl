(in-package :stack-dsl)

(defun path (filename)
  (asdf:system-relative-pathname :stack-dsl filename))
