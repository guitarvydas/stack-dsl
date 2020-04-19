
= stack-language
  ~rmSpaces
  existenceClear
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

= classWithFields 
  '{'
  fieldClear
  @fieldDefs 
  '}'
  fieldEmit

= orClass
  '|' 
  orPushNew
  SYMBOL 
  orAddSymbol
  existenceTypeSave
  {[ ?'|' '|' SYMBOL orAddSymbol existenceTypeSave | * > ]}
  orEmit

= bagDef SYMBOL/bag SYMBOL bagEmit
= mapDef SYMBOL/map SYMBOL mapEmit
= stringDef SYMBOL/string stringEmit

= fieldDefs
  @field
  {[ ?SYMBOL @field | * > ]}

= field
  SYMBOL 
  fieldPushNew 
  @optionalInitializer

= optionalInitializer
  [ ?'=' '=' CHARACTER/' SYMBOL fieldSetInit CHARACTER/'
  | *
  ]