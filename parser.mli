type token =
  | EOF
  | COMMA
  | NIL
  | CONS
  | ISNIL
  | HEAD
  | TAIL
  | LIST
  | ID of (Symbol.t)
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | NUM of (int)
  | LET
  | IN
  | COLON
  | SEMI
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE
  | MOD
  | CARET
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | AND
  | OR
  | NOT
  | EQ
  | ASSIGN
  | LT
  | LE
  | CMPEQ
  | NE
  | NEALT
  | GE
  | GT
  | ARROW
  | INTTYPE
  | BOOLTYPE
  | FUN
  | PI
  | DOT
  | STAR

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.term
