/* file: parser.mly */

%{
  open Printf
  let toId s = Ast.Ast.Var(Ast.Ast.makeString s)
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

%type <Ast.Ast.term> input
%start input

%% /* Grammar rules and actions follow */

input:
 exp SEMI SEMI EOF    { $1 }
;

exp: term             { $1 }
| exp term            { Ast.Ast.App($1, $2) }
;

term:      NUM        { Ast.Ast.Int($1) }
| TRUE                { Ast.Ast.Bool(1) }
| FALSE               { Ast.Ast.Bool(0) }
| FUN ID COLON tyterm DOT exp { Ast.Ast.Lambda(Ast.Ast.String($2), $4, $6) }
| tyterm              { $1 }
| ID                  { Ast.Ast.Var(Ast.Ast.String($1)) }
| ISNIL exp   	      { Ast.Ast.IsNil($2) }
| HEAD exp 	      { Ast.Ast.Head($2) }
| TAIL exp 	      { Ast.Ast.Tail($2) }
| list                { $1 }
| exp COLON exp       { Ast.Ast.Ann($1, $3) }
| exp PLUS exp        { let id = toId("+") in Ast.Ast.Op(id,[$1; $3]) }
| exp MINUS exp       { let id = toId("-") in Ast.Ast.Op(id,[$1; $3]) }
| exp MULTIPLY exp    { let id = toId("*") in Ast.Ast.Op(id,[$1; $3]) }
| exp DIVIDE exp      { let id = toId("/") in Ast.Ast.Op(id,[$1; $3]) }
| exp CARET exp       { let id = toId("**") in Ast.Ast.Op(id,[$1; $3]) }
| exp MOD exp         { let id = toId("%") in Ast.Ast.Op(id,[$1; $3]) }
| exp LT exp          { let id = toId("<") in Ast.Ast.Op(id,[$1; $3]) }
| exp LE exp          { let id = toId("<=") in Ast.Ast.Op(id,[$1; $3]) }
| exp CMPEQ exp       { let id = toId("==") in Ast.Ast.Op(id,[$1; $3]) }
| exp NE exp          { let id = toId("<>") in Ast.Ast.Op(id,[$1; $3]) }
| exp NEALT exp       { let id = toId("<>") in Ast.Ast.Op(id,[$1; $3]) }
| exp GT exp          { let id = toId(">") in Ast.Ast.Op(id,[$1; $3]) }
| exp GE exp          { let id = toId(">=") in Ast.Ast.Op(id,[$1; $3]) }
| NOT exp             { let id = toId("not") in Ast.Ast.Op(id,[$2]) }
| MINUS exp           { let id = toId("-") in Ast.Ast.Op(id,[Ast.Ast.Int(0); $2]) }
| LPAREN exp RPAREN   { $2 }
| IF exp THEN exp ELSE exp { Ast.Ast.If($2,$4,$6) }
| exp AND exp         { Ast.Ast.And($1, $3) }
| exp OR exp          { Ast.Ast.Or($1, $3) }
| LET ID COLON tyterm EQ exp	   { Ast.Ast.Let(Ast.Ast.String($2), $4, $6) }
;

list :
 NIL LBRACKET tyterm RBRACKET { Ast.Ast.Nil($3) }
| CONS LBRACKET term COMMA tyterm RBRACKET exp term { Ast.Ast.Cons($3, $5, $7, $8) }
;

tyterm:
  INTTYPE            { Ast.Ast.IntType }
| BOOLTYPE           { Ast.Ast.BoolType }
| PI ID COLON tyterm DOT tyterm  { Ast.Ast.Pi(Ast.Ast.String($2), $4, $6) }
| tyterm LIST LPAREN term RPAREN { Ast.Ast.List($1, $4) }
| LPAREN tyterm RPAREN   { $2 }
| ID                 { Ast.Ast.Var(Ast.Ast.String($1)) }
| STAR               { Ast.Ast.Star }
;

%%
