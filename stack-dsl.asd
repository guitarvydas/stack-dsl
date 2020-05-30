(defsystem :stack-dsl
  :depends-on (:loops :alexandria :cl-json :parsing-assembler/use)
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")
				     (:file "classes")
				     (:file "path")))))
			
(defsystem :stack-dsl/generate
  :depends-on (:stack-dsl)
  :components ((:module "source"
                        :pathname "./"
                        :components ((:static-file "stack-dsl.pasm")
				     (:file "generate" :depends-on ("stack-dsl.pasm"))))))

(defsystem :stack-dsl/use
  :depends-on (:stack-dsl :cl-ppcre :loops)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3)
                                         (safety 3)
                                         (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "support")
				     (:file "type-descriptor")
				     (:file "mechanisms")
				     (:file "transpile")
				     (:file "pattern-matching-primitives")
				     (:file "patternmatch"
					    :depends-on ("pattern-matching-primitives"))
				     (:file "transpile-mechanisms" 
					    :depends-on ("patternmatch"
							 "pattern-matching-primitives"))
				     (:file "stack-dsl" 
					    :depends-on ("support"
							 "type-descriptor"
							 "mechanisms"
							 "transpile"
							 "pattern-matching-primitives"
							 "patternmatch"
							 "transpile-mechanisms"
							 ))))))

