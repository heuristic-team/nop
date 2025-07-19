program -> typeDeclaration linesep program | function linesep program | E

typeDeclaration -> "type" id "=" typeEntrances

function -> "fn" id "(" params ")" maybeType "=" body

stmt -> declare | assign | return | expr

expr -> call | binary | lambda | ifrule | for |"(" expr ")" | literal

ifrule -> "if" expr "then" body | "if" expr "then" body "else" body 
    | "if" expr "then" body "else" ifrule

for -> for maybeDeclare; maybeCond; maybeInc do body

call -> id "(" args ")"
binary -> expr op expr

return -> "return" maybeExpr

declare -> id ":" maybeType "=" expr
assign -> id "=" expr

lambda -> "(\" args "->" body ")""

body -> stmt linesep body | stmt

maybeExpr -> expr | E
maybeDeclare -> declare | E
maybeCond -> expr | E
maybeInc -> expr | E
maybeType -> type | E
maybeString -> string | E

typeEntrance -> id ":" type | id ":" linesep typeEntrances

typeEntrances -> typeEntrance "and" typeEntrances
               | typeEntrance "or" typeEntrances
               | typeEntrance
type -> id
params -> id type, params | id type | E
args -> id, args | id | E
id -> string
unit -> "()"

literal -> unit | number | "\"" maybeString "\"" | "true" | "false"
