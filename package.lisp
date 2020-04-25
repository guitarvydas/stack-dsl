(defpackage :stack-dsl
  (:use :cl :parsing-assembler)
  (:export
   #:transpile-stack
   #:%ensure-existence

   #:%ensure-type
   #:%ensure-appendable-type
   #:%output
   #:%pop
   #:%push-empty
   #:%check-type
   #:%replace-top
   #:%type
   #:%element-type
   #:%field--type
   #:%set-field
   #:%get-field
   #:%append))
