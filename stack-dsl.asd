(defsystem :stack-dsl
  :depends-on (:loops :alexandria :parsing-assembler)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")
				     (:file "internal-support" :depends-on ("package"))
				     (:file "dsl" :depends-on ("package" "internal-support"))))))

(defsystem :stack-dsl/test
  :depends-on (:stack-dsl)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "test")))))
    

