(in-package "STACK-DSL")

(defmethod stack-language ((p pasm:parser))
  (let ((prev-rule (current-rule p)))     (setf (current-rule p) "stack-language") (pasm::p-into-trace p)
(pasm::pasm-filter-stream p #'rmSpaces)
(pasm:call-external p #'existenceClear)
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
(setf (current-rule p) prev-rule) (pasm::p-return-trace p)))

(defmethod rmSpaces ((p pasm:parser))
  (let ((prev-rule (current-rule p)))     (setf (current-rule p) "rmSpaces") (pasm::p-into-trace p)
(cond
((pasm:parser-success? (pasm:lookahead? p :SPACE)))
((pasm:parser-success? (pasm:lookahead? p :COMMENT)))
( t  (pasm:accept p) 
)
)

(setf (current-rule p) prev-rule) (pasm::p-return-trace p)))

(defmethod dsl-tail ((p pasm:parser))
  (let ((prev-rule (current-rule p)))     (setf (current-rule p) "dsl-tail") (pasm::p-into-trace p)
(cond
((pasm:parser-success? (pasm:lookahead-char? p #\{))(pasm:call-rule p #'classWithFields)
)
((pasm:parser-success? (pasm:lookahead-char? p #\|))(pasm:call-rule p #'orClass)
)
((pasm:parser-success? (pasm:lookahead-char? p #\:))(pasm:call-rule p #'colonTail)
)
((pasm:parser-success? (pasm:lookahead-char? p #\'))(pasm:call-rule p #'enum)
)
( t (pasm:call-external p #'errorTail)
)
)

(setf (current-rule p) prev-rule) (pasm::p-return-trace p)))

(defmethod colonTail ((p pasm:parser))
  (let ((prev-rule (current-rule p)))     (setf (current-rule p) "colonTail") (pasm::p-into-trace p)
(pasm:input-char p #\:)
(cond
((pasm:parser-success? (pasm:lookahead-symbol? p "bag"))(pasm:call-rule p #'bagDef)
)
((pasm:parser-success? (pasm:lookahead-symbol? p "string"))(pasm:call-rule p #'stringDef)
)
((pasm:parser-success? (pasm:lookahead-symbol? p "map"))(pasm:call-rule p #'mapDef)
)
( t (pasm:call-external p #'errorColonTail)
)
)

(setf (current-rule p) prev-rule) (pasm::p-return-trace p)))

(defmethod classWithFields ((p pasm:parser))
  (let ((prev-rule (current-rule p)))     (setf (current-rule p) "classWithFields") (pasm::p-into-trace p)
(pasm:input-char p #\{)
(pasm:call-external p #'fieldClear)
(pasm:call-rule p #'fieldDefs)
(pasm:input-char p #\})
(pasm:call-external p #'fieldEmit)
(setf (current-rule p) prev-rule) (pasm::p-return-trace p)))

(defmethod orClass ((p pasm:parser))
  (let ((prev-rule (current-rule p)))     (setf (current-rule p) "orClass") (pasm::p-into-trace p)
(pasm:input-char p #\|)
(pasm:call-external p #'orPushNew)
(pasm:input p :SYMBOL)
(pasm:call-external p #'orAddSymbol)
(pasm:call-external p #'existenceTypeSave)
(loop
(cond
((pasm:parser-success? (pasm:lookahead-char? p #\|))(pasm:input-char p #\|)
(pasm:input p :SYMBOL)
(pasm:call-external p #'orAddSymbol)
(pasm:call-external p #'existenceTypeSave)
)
( t (return)
)
)

)

(pasm:call-external p #'orEmit)
(setf (current-rule p) prev-rule) (pasm::p-return-trace p)))

(defmethod bagDef ((p pasm:parser))
  (let ((prev-rule (current-rule p)))     (setf (current-rule p) "bagDef") (pasm::p-into-trace p)
(pasm:input-symbol p "bag")
(pasm:input p :SYMBOL)
(pasm:call-external p #'bagEmit)
(setf (current-rule p) prev-rule) (pasm::p-return-trace p)))

(defmethod mapDef ((p pasm:parser))
  (let ((prev-rule (current-rule p)))     (setf (current-rule p) "mapDef") (pasm::p-into-trace p)
(pasm:input-symbol p "map")
(pasm:input p :SYMBOL)
(pasm:call-external p #'mapEmit)
(setf (current-rule p) prev-rule) (pasm::p-return-trace p)))

(defmethod stringDef ((p pasm:parser))
  (let ((prev-rule (current-rule p)))     (setf (current-rule p) "stringDef") (pasm::p-into-trace p)
(pasm:input-symbol p "string")
(pasm:call-external p #'stringEmit)
(setf (current-rule p) prev-rule) (pasm::p-return-trace p)))

(defmethod fieldDefs ((p pasm:parser))
  (let ((prev-rule (current-rule p)))     (setf (current-rule p) "fieldDefs") (pasm::p-into-trace p)
(pasm:call-rule p #'field)
(loop
(cond
((pasm:parser-success? (pasm:lookahead? p :SYMBOL))(pasm:call-rule p #'field)
)
( t (return)
)
)

)

(setf (current-rule p) prev-rule) (pasm::p-return-trace p)))

(defmethod field ((p pasm:parser))
  (let ((prev-rule (current-rule p)))     (setf (current-rule p) "field") (pasm::p-into-trace p)
(pasm:input p :SYMBOL)
(pasm:call-external p #'fieldPushNew)
(pasm:call-external p #'existenceTypeSave)
(pasm:call-rule p #'optionalInitializer)
(setf (current-rule p) prev-rule) (pasm::p-return-trace p)))

(defmethod optionalInitializer ((p pasm:parser))
  (let ((prev-rule (current-rule p)))     (setf (current-rule p) "optionalInitializer") (pasm::p-into-trace p)
(cond
((pasm:parser-success? (pasm:lookahead-char? p #\=))(pasm:input-char p #\=)
(pasm:input-char p #\')
(pasm:input p :SYMBOL)
(pasm:call-external p #'fieldSetInit)
(pasm:input-char p #\')
)
( t )
)

(setf (current-rule p) prev-rule) (pasm::p-return-trace p)))

(defmethod emun ((p pasm:parser))
  (let ((prev-rule (current-rule p)))     (setf (current-rule p) "emun") (pasm::p-into-trace p)
(pasm:call-external p #'enumPushNew)
(pasm:call-rule p #'constant)
(loop
(cond
((pasm:parser-success? (pasm:lookahead-char? p #\'))(pasm:call-rule p #'constant)
)
( t (return)
)
)

)

(pasm:call-external p #'enumEmit)
(setf (current-rule p) prev-rule) (pasm::p-return-trace p)))

(defmethod constant ((p pasm:parser))
  (let ((prev-rule (current-rule p)))     (setf (current-rule p) "constant") (pasm::p-into-trace p)
(pasm:input-char p #\')
(pasm:input p :SYMBOL)
(pasm:call-external p #'enumPushSymbol)
(pasm:input-char p #\')
(setf (current-rule p) prev-rule) (pasm::p-return-trace p)))

