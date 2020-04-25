(defpackage :stack-dsl
  (:use :cl :parsing-assembler)
  (:export
   #:transpile-stack
   #:%ensure-existence

   #:%string
   #:val
   #:%stack
   
   #:%ensure-type
   #:%ensure-appendable-type
   #:%output
   #:%pop
   #:%top
   #:%push-empty
   #:%check-type
   #:%replace-top
   #:%type
   #:%element-type
   #:%field--type
   #:%set-field
   #:%get-field
   #:%append))
