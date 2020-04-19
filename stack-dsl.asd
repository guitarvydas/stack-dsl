(defsystem :stack-dsl
  :depends-on (:loops :alexandria :parsing-assembler/use)
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")))))
			
(defsystem :stack-dsl/use
  :depends-on (:stack-dsl)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components (
				     (:file "classes")
				     (:file "internal-support" :depends-on ("classes"))
				     (:file "mechanisms" :depends-on ("classes"))
				     (:static-file "pasm.pasm")
				     (:static-file "stack.dsl")
				     (:file "main" :depends-on (
								"classes"
								"pasm.pasm"
								"stack.dsl"
								"internal-support"
								"mechanisms"
								))))))

(defsystem :stack-dsl/test
  :depends-on (:stack-dsl/use)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "test")))))
    

