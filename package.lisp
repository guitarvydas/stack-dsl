(defpackage :stack-dsl
  (:use :cl :parsing-assembler)
  (:export
   #:transpile-stack
   #:%ensure-existence

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
   #:%check-type
   #:%type
   #:%element-type
   #:%field-type
   #:%set-field
   #:%get-field
   #:%append))
