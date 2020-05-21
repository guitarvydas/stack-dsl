
= stack-language
  ~rmSpaces
                         $alist__clear
  {[ ?SYMBOL
    SYMBOL
                         $alist__saveName
    '='
    @dsl-tail
   | >
  ]}
  EOF

= rmSpaces
  [ ?SPACE | ?COMMENT | * . ]

= dsl-tail
  [ ?'{' @classWithFields
  | ?'|' @compoundClass
  | ?':' @colonTail
  | ?CHARACTER/' @enumTail
  | * errorTail
  ]

= colonTail
  ':'
  [ ?SYMBOL/bag @bagDef
  | ?SYMBOL/string @stringDef
  | ?SYMBOL/null @nullDef
  | ?SYMBOL/map @mapDef
  | * errorColonTail
  ]

= classWithFields 
  '{'
  @fieldDefs 
  '}'
  fieldEmit

= compoundClass
  '|' 
  SYMBOL 
  {[ ?'|' '|' SYMBOL | * > ]}

= bagDef SYMBOL/bag SYMBOL
= mapDef SYMBOL/map SYMBOL
= stringDef SYMBOL/string
= nullDef SYMBOL/null

= fieldDefs
  @field
  {[ ?SYMBOL @field | * > ]}

= field
  SYMBOL 
  @optionalInitializer

= optionalInitializer
  [ ?'=' '=' CHARACTER/' SYMBOL CHARACTER/'
  | *
  ]

= enumTail
  @constant
  {[ ?'|' '|' @constant
  | * >
  ]}

= constant
  CHARACTER/'
  SYMBOL
  CHARACTER/'
  