(defpackage :stack-dsl
  (:use :cl :parsing-assembler)
  (:export
   #:transpile-stack
   #:%ensure-existence
   #:lisp-sym

   #:%string
   #:%value
   #:%stack
   
   #:%ensure-type
   #:%ensure-field-type
   #:%ensure-appendable-type
   #:%output
   #:%pop
   #:%top
   #:%push-empty
   #:%push
   #:%check-type
   #:%type
   #:%element-type
   #:%field-type
   #:%set-field
   #:%get-field
   #:%append

   ;; macros for using the environment stacks
   #:~output       ;; move top(xxx-input) to top(xxx-output), pop xxx
   #:~newscope     ;; push empty onto xxx stack
   #:~append       ;; set top(xxx-input) to append(xxx-input,top(output-other)), pop output-other, top(xxx-input) must be a :bag or a :map
   #:~set-field    ;; set top(xxx-input).field to top(output-other), pop output-other
   ))
