(in-package :stack-dsl)

(defparameter *use-dsl*
  "
machineDescriptor = { name initiallyDescriptor states }
initiallyDescriptor = :bag statement
states = :bag state
state = { name events }
events = :bag event
event = { onName statements }
onName = :string

statements = :bag statement
statement = | sendStatement | callStatement
sendStatement = { kind='send' expr }
callStatement = { kind='call' exprmap }
exprmap = :map expr
expr = | rawExpr | dollarExpr | callExpr
dollarExpr = { kind='dollar' }
callExpr = { kind='function' argmap }
rawExpr = { kind='raw' rawText }
rawText = :string
name = :string
")

;; patterns...

;; id = { ... }     --> class with fields def
;; id = :bag ...    --> bag def
;; id = :string ... --> string def
;; id = :map ...    --> map def
;; id = | ...       --> or type def

(defparameter *asm*
  "
= stack-language
  ~rmSpaces
  {[ ?SYMBOL
    SYMBOL
    '='
    @dsl-tail
   | >
  ]}
  EOF

= rmSpaces
  [ ?SPACE | ?COMMENT | * . ]

= dsl-tail
  [ ?'{' @classWithFields
  | ?'|' @orClass
  | ?':' @colonTail
  | * errorTail
  ]

= colonTail
  ':'
  [ ?SYMBOL/bag @bagDef
  | ?SYMBOL/string @stringDef
  | ?SYMBOL/map @mapDef
  | * errorColonTail
  ]

= classWithFields '{' @fieldDefs '}'
= orClass '|' SYMBOL {[ ?'|' '|' SYMBOL | * > ]}
= bagDef SYMBOL/bag SYMBOL
= mapDef SYMBOL/map SYMBOL
= stringDef SYMBOL/string

= fieldDefs
  @field
  {[ ?SYMBOL @field | * > ]}

= field
  SYMBOL @optionalInitializer

= optionalInitializer
  [ ?'=' '=' CHARACTER/' SYMBOL CHARACTER/'
  | *
  ]

")

(defclass stack-dsl-parser (pasm:parser)
  ())


(defparameter *internal-support* (alexandria:read-file-into-string 
				  (asdf:system-relative-pathname :stack-dsl "internal-support.lisp")))

;; mechanisms
(defmethod errorTail ((self stack-dsl-parser)) (error "tail error"))
(defmethod errorColonTail ((self stack-dsl-parser)) (error "colon tail error"))
;; end mechanisms
