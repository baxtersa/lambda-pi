/* file: parser.mly */

%{
  open Printf
  let toId s = Ast.Var(Ast.makeString s)
%}

%token EOF
%token COMMA
%token NIL CONS ISNIL HEAD TAIL LIST
%token <string> ID
%token LPAREN RPAREN LBRACKET RBRACKET
%token <int> NUM
%token LET IN COLON SEMI
%token PLUS MINUS MULTIPLY DIVIDE MOD CARET
%token TRUE FALSE IF THEN ELSE AND OR NOT EQ
%token LT LE CMPEQ NE NEALT GE GT
%token ARROW INTTYPE BOOLTYPE FUN PI DOT STAR

%nonassoc CONS NIL ISNIL HEAD TAIL
%nonassoc COLON SEMI FUN PI
%nonassoc IF THEN ELSE
%right DOT
%left LET
%right EQ
%right IN
%right ARROW
%left OR
%left AND 
%left LT LE CMPEQ NE NEALT GE GT
%left PLUS MINUS
%left MULTIPLY DIVIDE MOD
%right CARET
%right COMMA
%nonassoc NOT TRUE FALSE LPAREN LBRACKET 

%start input
%type <Ast.term> input

%% /* Grammar rules and actions follow */

input:
 exp SEMI SEMI EOF    { $1 }
;

exp: term             { $1 }
| exp term            { Ast.App($1, $2) }
;

term:      NUM        { Ast.Int($1) }
| TRUE                { Ast.Bool(1) }
| FALSE               { Ast.Bool(0) }
| FUN ID COLON tyterm DOT exp { Ast.Lambda(Ast.String($2), $4, $6) }
| tyterm              { $1 }
| ID                  { Ast.Var(Ast.String($1)) }
| ISNIL exp   	      { Ast.IsNil($2) }
| HEAD exp 	      { Ast.Head($2) }
| TAIL exp 	      { Ast.Tail($2) }
| list                { $1 }
| exp COLON exp       { Ast.Ann($1, $3) }
| exp PLUS exp        { let id = toId("+") in Ast.Op(id,[$1; $3]) }
| exp MINUS exp       { let id = toId("-") in Ast.Op(id,[$1; $3]) }
| exp MULTIPLY exp    { let id = toId("*") in Ast.Op(id,[$1; $3]) }
| exp DIVIDE exp      { let id = toId("/") in Ast.Op(id,[$1; $3]) }
| exp CARET exp       { let id = toId("**") in Ast.Op(id,[$1; $3]) }
| exp MOD exp         { let id = toId("%") in Ast.Op(id,[$1; $3]) }
| exp LT exp          { let id = toId("<") in Ast.Op(id,[$1; $3]) }
| exp LE exp          { let id = toId("<=") in Ast.Op(id,[$1; $3]) }
| exp CMPEQ exp       { let id = toId("==") in Ast.Op(id,[$1; $3]) }
| exp NE exp          { let id = toId("<>") in Ast.Op(id,[$1; $3]) }
| exp NEALT exp       { let id = toId("<>") in Ast.Op(id,[$1; $3]) }
| exp GT exp          { let id = toId(">") in Ast.Op(id,[$1; $3]) }
| exp GE exp          { let id = toId(">=") in Ast.Op(id,[$1; $3]) }
| NOT exp             { let id = toId("not") in Ast.Op(id,[$2]) }
| MINUS exp           { let id = toId("-") in Ast.Op(id,[Ast.Int(0); $2]) }
| LPAREN exp RPAREN   { $2 }
| IF exp THEN exp ELSE exp { Ast.If($2,$4,$6) }
| exp AND exp         { Ast.And($1, $3) }
| exp OR exp          { Ast.Or($1, $3) }
| LET ID COLON tyterm EQ exp	   { Ast.Let(Ast.String($2), $4, $6) }
;

list :
 NIL LBRACKET tyterm RBRACKET { Ast.Nil($3) }
| CONS LBRACKET term COMMA tyterm RBRACKET exp term { Ast.Cons($3, $5, $7, $8) }
;

tyterm:
  INTTYPE            { Ast.IntType }
| BOOLTYPE           { Ast.BoolType }
| PI ID COLON tyterm DOT tyterm  { Ast.Pi(Ast.String($2), $4, $6) }
| tyterm LIST LPAREN term RPAREN { Ast.List($1, $4) }
| LPAREN tyterm RPAREN   { $2 }
| ID                 { Ast.Var(Ast.String($1)) }
| STAR               { Ast.Star }
;

%%
