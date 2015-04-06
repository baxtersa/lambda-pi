(* file: lexer.mll *)
(* Lexical analyzer returns one of the tokens:
   the token NUM of integer,
   operators (PLUS, MINUS, MULTIPLY, DIVIDE, CARET),
   or EOF.  It skips all blanks and tabs, unknown characters. *)
{
  open Parser (* Assumes the parser file is "parser.mly". *)
}
let digit = ['0'-'9']
let word = ['a'-'z''A'-'Z']
rule token = parse
  | [' ' '\t' '\n']{ token lexbuf }
  | ','     { COMMA }
  | digit+
  | "." digit+
  | digit+ "." digit* as num
      { NUM (int_of_string num) }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { MULTIPLY }
  | '/'     { DIVIDE }
  | '%'     { MOD }
  | ':'     { COLON }
  | ';'     { SEMI }
  | "->"    { ARROW }
  | "fn"    { FUN }
  | "pi"    { PI }
  | "~"     { STAR }
  | '.'     { DOT }
  | "int"   { INTTYPE }
  | "bool"  { BOOLTYPE }
  | "list"  { LIST }
  | "is_nil"{ ISNIL }
  | "head"  { HEAD }
  | "tail"  { TAIL }
  | "stop"  { NIL }
  | "more"  { CONS }
  | "**"    { CARET }
  | "<"     { LT }
  | "<="    { LE }
  | "="     { EQ }
  | "=="    { CMPEQ }
  | "<>"    { NE }
  | "!="    { NEALT }
  | ">="    { GE }
  | ">"     { GT }
  | "true"  { TRUE }
  | "false" { FALSE }
  | "let"   { LET }
  | "in"    { IN }
  | "if"    { IF }
  | "else"  { ELSE }
  | "then"  { THEN }
  | "and"   { AND }
  | "or"    { OR }
  | "not"   { NOT }
  | word+ as string { ID string }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '['     { LBRACKET }
  | ']'     { RBRACKET }
  | _       { token lexbuf }
  | eof     { EOF }
