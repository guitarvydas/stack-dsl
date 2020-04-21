machineDescriptor = { name initiallyDescriptor states }
initiallyDescriptor = :bag statement
statesBlock = :bag state
state = { name events }
eventsBlock = :bag event
event = { onName statements }
onName = :string

statementsBlock = :bag statement
statement = | sendStatement | callStatement
sendStatement = { kind='send' expr }
callStatement = { kind='call' exprmap }
exprBlock = :map expr
expr = | rawExpr | dollarExpr | callExpr
dollarExpr = { kind='dollar' }
callExpr = { kind='function' argmap }
rawExpr = { kind='raw' rawText }
rawText = :string
name = :string


