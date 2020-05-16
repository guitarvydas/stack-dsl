
= stack-language
  ~rmSpaces
  headerEmit
  {[ ?SYMBOL
    SYMBOL
    symbolSave 
    existenceTypeSave
    '='
    @dsl-tail
   | >
  ]}
  EOF
  existenceEmit
  environmentEmit

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
  fieldClear
  @fieldDefs 
  '}'
  fieldEmit

= compoundClass
  '|' 
  compoundPushNew
  SYMBOL 
  compoundAddSymbol
  existenceTypeSave
  {[ ?'|' '|' SYMBOL compoundAddSymbol existenceTypeSave | * > ]}
  compoundEmit

= bagDef SYMBOL/bag SYMBOL bagEmit
= mapDef SYMBOL/map SYMBOL mapEmit
= stringDef SYMBOL/string stringEmit
= nullDef SYMBOL/null nullEmit

= fieldDefs
  @field
  {[ ?SYMBOL @field | * > ]}

= field
  SYMBOL 
  fieldPushNew existenceTypeSave
  @optionalInitializer

= optionalInitializer
  [ ?'=' '=' CHARACTER/' SYMBOL fieldSetInit CHARACTER/'
  | *
  ]

= enumTail
  enumPushNew
  @constant
  {[ ?'|' '|' @constant
  | * >
  ]}
  enumEmit

= constant
  CHARACTER/'
  SYMBOL
  enumPushSymbol
  CHARACTER/'
  