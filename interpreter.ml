
module type INTERPRETER = 
sig
  open Ast
  open Environment
  val interpreter : Environment.context ref -> unit
  val makeContext : ('a * 'b) list -> ('c * 'd) list -> ('a * ('b * 'd option)) list
  val ctx : Environment.context ref
end;;

module Interpreter : INTERPRETER =
struct
  open Basis;;
  open Staticsemantics;;
  open Environment;;
  
  let stBasis = Basis.Interpreter.staticBasis;;
  let dyBasis = Basis.Interpreter.dynamicBasis;;
  
  let parseInput() =
    let inch = input_line stdin in
    let lexbuf = Lexing.from_string inch in
    let ast = Parser.input Lexer.token lexbuf in
    ast;;
  
  let rec interpreter context : unit =
    (
      output_string stdout ("\nlpi> ");
      flush stdout;
      
      try 
        (let ast = parseInput()
         in (try
	       (
		 let e = Staticsemantics.infer context ast in
		 let t, ctx' = Staticsemantics.normalize context ast in
		 (
		   output_string stdout (Ast.toString t);
		   output_string stdout ":";
		   output_string stdout (Ast.toString e);
		   output_string stdout "\n";
		   flush stdout;
		   interpreter ctx'
		 )
	       )
	   with Failure s ->
	     (
	       output_string stdout s;
	       flush stdout;
	       interpreter context
	     )
	 )
	)
      with 
      | Parsing.Parse_error ->
	(
	  output_string stdout "Input string does not parse...\n";
	  flush stdout;
	  interpreter context
	)
      | Failure s ->
	(
	  output_string stdout "Input does not type-check...\n";
	  flush stdout;
	  interpreter context
	)
    );;

  let rec makeContext x y = (match x, y with
    | [], [] -> []
    | (a, b)::s, (c, d)::t -> (a, (b, Some d))::(makeContext s t));;

  let ctx = ref (makeContext stBasis dyBasis);;

  let _ = interpreter ctx;;

end;;
