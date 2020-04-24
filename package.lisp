(defpackage :stack-dsl
  (:use :cl :parsing-assembler)
  (:export
   #:transpile-stack
   #:%check-existence

   #:%check-type
   #:%check-appendable-type
   #:%output
   #:%pop
   #:%push-empty
   #:%check-type
   #:%replace-top
   #:%type
   #:%element-type
   #:%set-field
   #:%get-field
   #:%append))
