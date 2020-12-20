(in-package "STACK-DSL")
(proclaim '(optimize (debug 3) (safety 3) (speed 0)))


(defmethod stack-language ((p pasm:parser))
     (setf (pasm:current-rule p) "stack-language")
(pasm::pasm-filter-stream p #'rmSpaces)
(pasm:call-external p #'headerEmit)
(loop
(cond
((pasm:parser-success? (pasm:lookahead? p :SYMBOL))(pasm:input p :SYMBOL)
(pasm:call-external p #'symbolSave)
(pasm:call-external p #'existenceTypeSave)
(pasm:input-char p #\=)
(pasm:call-rule p #'dsl-tail)
)
((return)
)
)

)

(pasm:input p :EOF)
(pasm:call-external p #'existenceEmit)
(pasm:call-external p #'environmentEmit)
)

(defmethod rmSpaces ((p pasm:parser))
     (setf (pasm:current-rule p) "rmSpaces")
(cond
((pasm:parser-success? (pasm:lookahead? p :SPACE)))
((pasm:parser-success? (pasm:lookahead? p :COMMENT)))
( t  (pasm:accept p) 
)
)

)

(defmethod dsl-tail ((p pasm:parser))
     (setf (pasm:current-rule p) "dsl-tail")
(cond
((pasm:parser-success? (pasm:lookahead-char? p #\{))(pasm:call-rule p #'classWithFields)
)
((pasm:parser-success? (pasm:lookahead-char? p #\|))(pasm:call-rule p #'compoundClass)
)
((pasm:parser-success? (pasm:lookahead-char? p #\:))(pasm:call-rule p #'colonTail)
)
((pasm:parser-success? (pasm:lookahead-char? p #\'))(pasm:call-rule p #'enumTail)
)
( t (pasm:call-external p #'errorTail)
)
)

)

(defmethod colonTail ((p pasm:parser))
     (setf (pasm:current-rule p) "colonTail")
(pasm:input-char p #\:)
(cond
((pasm:parser-success? (pasm:lookahead-symbol? p "bag"))(pasm:call-rule p #'bagDef)
)
((pasm:parser-success? (pasm:lookahead-symbol? p "string"))(pasm:call-rule p #'stringDef)
)
((pasm:parser-success? (pasm:lookahead-symbol? p "null"))(pasm:call-rule p #'nullDef)
)
((pasm:parser-success? (pasm:lookahead-symbol? p "map"))(pasm:call-rule p #'mapDef)
)
( t (pasm:call-external p #'errorColonTail)
)
)

)

(defmethod classWithFields ((p pasm:parser))
     (setf (pasm:current-rule p) "classWithFields")
(pasm:input-char p #\{)
(pasm:call-external p #'fieldClear)
(pasm:call-rule p #'fieldDefs)
(pasm:input-char p #\})
(pasm:call-external p #'fieldEmit)
)

(defmethod compoundClass ((p pasm:parser))
     (setf (pasm:current-rule p) "compoundClass")
(pasm:input-char p #\|)
(pasm:call-external p #'compoundPushNew)
(pasm:input p :SYMBOL)
(pasm:call-external p #'compoundAddSymbol)
(pasm:call-external p #'existenceTypeSave)
(loop
(cond
((pasm:parser-success? (pasm:lookahead-char? p #\|))(pasm:input-char p #\|)
(pasm:input p :SYMBOL)
(pasm:call-external p #'compoundAddSymbol)
(pasm:call-external p #'existenceTypeSave)
)
( t (return)
)
)

)

(pasm:call-external p #'compoundEmit)
)

(defmethod bagDef ((p pasm:parser))
     (setf (pasm:current-rule p) "bagDef")
(pasm:input-symbol p "bag")
(pasm:input p :SYMBOL)
(pasm:call-external p #'bagEmit)
)

(defmethod mapDef ((p pasm:parser))
     (setf (pasm:current-rule p) "mapDef")
(pasm:input-symbol p "map")
(pasm:input p :SYMBOL)
(pasm:call-external p #'mapEmit)
)

(defmethod stringDef ((p pasm:parser))
     (setf (pasm:current-rule p) "stringDef")
(pasm:input-symbol p "string")
(pasm:call-external p #'stringEmit)
)

(defmethod nullDef ((p pasm:parser))
     (setf (pasm:current-rule p) "nullDef")
(pasm:input-symbol p "null")
(pasm:call-external p #'nullEmit)
)

(defmethod fieldDefs ((p pasm:parser))
     (setf (pasm:current-rule p) "fieldDefs")
(pasm:call-rule p #'field)
(loop
(cond
((pasm:parser-success? (pasm:lookahead? p :SYMBOL))(pasm:call-rule p #'field)
)
( t (return)
)
)

)

)

(defmethod field ((p pasm:parser))
     (setf (pasm:current-rule p) "field")
(pasm:input p :SYMBOL)
(pasm:call-external p #'fieldPushNew)
(pasm:call-external p #'existenceTypeSave)
(pasm:call-rule p #'optionalInitializer)
)

(defmethod optionalInitializer ((p pasm:parser))
     (setf (pasm:current-rule p) "optionalInitializer")
(cond
((pasm:parser-success? (pasm:lookahead-char? p #\=))(pasm:input-char p #\=)
(pasm:input-char p #\')
(pasm:input p :SYMBOL)
(pasm:call-external p #'fieldSetInit)
(pasm:input-char p #\')
)
( t )
)

)

(defmethod enumTail ((p pasm:parser))
     (setf (pasm:current-rule p) "enumTail")
(pasm:call-external p #'enumPushNew)
(pasm:call-rule p #'constant)
(loop
(cond
((pasm:parser-success? (pasm:lookahead-char? p #\|))(pasm:input-char p #\|)
(pasm:call-rule p #'constant)
)
( t (return)
)
)

)

(pasm:call-external p #'enumEmit)
)

(defmethod constant ((p pasm:parser))
     (setf (pasm:current-rule p) "constant")
(pasm:input-char p #\')
(pasm:input p :SYMBOL)
(pasm:call-external p #'enumPushSymbol)
(pasm:input-char p #\')
)

