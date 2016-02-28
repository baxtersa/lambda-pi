/* file: parser.mly */

%{
       open Printf
       let toId s = Ast.Ast.Var(Ast.Ast.makeString s)
%}

%token SEMI
%token COMMA ARROW
%token LPAREN RPAREN LBRACKET RBRACKET
%token <int> NUM
%token TRUE FALSE
%token <string> ID
%token INTTYPE BOOLTYPE STAR LIST
%token PLUS MINUS MULTIPLY DIVIDE MOD CARET
%token ISNIL HEAD TAIL NIL CONS
%token IF THEN ELSE AND OR NOT EQ
%token LT LE CMPEQ NE NEALT GE GT
%token LET IN COLON FUN PI DOT
%token EOF

%right DOT
       
%type <Ast.Ast.term> input
%start input

%% /* Grammar rules and actions follow */

  input:
| exp SEMI SEMI EOF	{ $1 }
| exp SEMI SEMI input	{ Ast.Ast.Seq($1, $4) }
;

  exp:
| operator		{ $1 }
| func			{ $1 }
| let_stmt		{ $1 }
| if_stmt		{ $1 }
;

  operator:
| operand		    { $1 }
| operator PLUS operand     { let id = toId("+") in Ast.Ast.Op(id,[$1; $3]) }
| operator MINUS operand    { let id = toId("-") in Ast.Ast.Op(id,[$1; $3]) }
| operator MULTIPLY operand { let id = toId("*") in Ast.Ast.Op(id,[$1; $3]) }
| operator DIVIDE operand   { let id = toId("/") in Ast.Ast.Op(id,[$1; $3]) }
| operator CARET operand    { let id = toId("**") in Ast.Ast.Op(id,[$1; $3]) }
| operator MOD operand      { let id = toId("%") in Ast.Ast.Op(id,[$1; $3]) }
| operator LT operand       { let id = toId("<") in Ast.Ast.Op(id,[$1; $3]) }
| operator LE operand       { let id = toId("<=") in Ast.Ast.Op(id,[$1; $3]) }
| operator CMPEQ operand    { let id = toId("==") in Ast.Ast.Op(id,[$1; $3]) }
| operator NE operand       { let id = toId("<>") in Ast.Ast.Op(id,[$1; $3]) }
| operator NEALT operand    { let id = toId("<>") in Ast.Ast.Op(id,[$1; $3]) }
| operator GT operand       { let id = toId(">") in Ast.Ast.Op(id,[$1; $3]) }
| operator GE operand       { let id = toId(">=") in Ast.Ast.Op(id,[$1; $3]) }
| operator AND operand      { Ast.Ast.And($1, $3) }
| operator OR operand       { Ast.Ast.Or($1, $3) }
| NOT operand	       	    { let id = toId("not") in Ast.Ast.Op(id,[$2]) }
| MINUS operand		    { let id = toId("-") in Ast.Ast.Op(id,[Ast.Ast.Int(0); $2]) }
;

  func :
| FUN ID COLON tyterm DOT exp	{ Ast.Ast.Lambda(Ast.Ast.String($2), $4, $6) }
;

  let_stmt:
| LET ID COLON tyterm EQ exp	{ Ast.Ast.Let(Ast.Ast.String($2), $4, $6) }
;

  if_stmt:
| IF exp THEN exp ELSE exp	{ Ast.Ast.If($2, $4, $6) }
;
     
  operand :
| primitive		{ $1 }
| operand primitive	{ Ast.Ast.App($1, $2) }
;

  primitive:
| NUM			{ Ast.Ast.Int($1) }
| TRUE			{ Ast.Ast.Bool(1) }
| FALSE			{ Ast.Ast.Bool(0) }
| tyterm		{ $1 }
;

  tyterm:
| INTTYPE		{ Ast.Ast.IntType }
| BOOLTYPE		{ Ast.Ast.BoolType }
| STAR			{ Ast.Ast.Star }
| ID			{ toId $1 }
| tyterm LIST LPAREN exp RPAREN		{ Ast.Ast.List($1, $4) }
| PI ID COLON tyterm DOT tyterm		{ Ast.Ast.Pi(Ast.Ast.String($2), $4, $6) }
| LPAREN exp RPAREN	{ $2 }
;

%%
