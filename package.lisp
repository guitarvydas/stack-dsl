(defpackage :stack-dsl
  (:use :cl :parsing-assembler)
  (:export
   #:transpile-stack
   #:%ensure-existence
   #:lisp-sym

   #:%string
   #:%value
   #:%stack
   #:%map
   
   #:%typed-value

   #:initialize-types
   #:%ensure-type
   #:%ensure-field-type
   #:%ensure-appendable-type
   #:%ensure-enum-value
   #:%output
   #:%pop
   #:%top
   #:%set-top
   #:%push-empty
   #:%push
   #:%check-type
   #:%type
   #:%element-type
   #:%field-type
   #:%set-field
   #:%get-field
   #:%append
   #:%ordered-list

   #:%make-empty-map
   #:%empty-p
   
   ;; macros for using the environment stacks
   #:~output       ;; move top(xxx-input) to top(xxx-output), pop xxx
   #:~newscope     ;; push empty onto xxx stack
   #:~append       ;; set top(xxx-input) to append(xxx-input,top(output-other)), pop output-other, top(xxx-input) must be a :bag or a :map
   #:~set-field    ;; set top(xxx-input).field to top(output-other), pop output-other

   #:set-target-package

   #:%list
   #:%copy-list
   #:%as-string

   #:tolower
   #:make-map-from-list
   #:make-typed-value
   #:make-typed-string
   ))
