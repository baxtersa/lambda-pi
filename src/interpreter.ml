module type INTERPRETER = 
sig
  open Ast
  open Environment
  val interpreter : Environment.context ref -> unit
  val makeContext : ('a * 'b) list -> ('c * 'd) list -> ('a * ('b * 'd option)) list
  val ctx : Environment.context ref
  val quiet : bool ref
end;;

module Interpreter : INTERPRETER =
struct
  open Ast;;
  open Basis;;
  open Staticsemantics;;
  open Environment;;
  open Interpret;;
  
  let stBasis = Basis.Interpreter.staticBasis;;
  let dyBasis = Basis.Interpreter.dynamicBasis;;

  let quiet = ref false;;
  let str_out = ref "";;

  let in_channel =
    if Array.length Sys.argv == 1
    then 
      (
	quiet := false;
	ref stdin;
      )
    else
      (
	quiet := true;
	ref (open_in Sys.argv.(1));
      );;
  
  let parseInput() =
    let end_of_line = ";;" in
    let rec getline() =
      let line = 
	try input_line !in_channel
	with 
	  End_of_file ->
	    (
	      close_in !in_channel;
	      in_channel := stdin;
	      quiet := false;
	      output_string stdout !str_out;
	      flush stdout;
	      raise Exit;
	    )
      in
      let line_term = 
	let substring = 
	  if String.length line < 2 then ""
	  else String.sub line (String.length line - 2) 2 in
	substring in
      if String.compare line_term end_of_line == 0
      then line
      else String.concat " " [line; getline()] in
    getline() |> Interpret.ast_from_string;;
    
  let rec interpreter context : unit =
    (
      if not !quiet
      then
	(
	  output_string stdout ("\nlpi> ");
	  flush stdout;
	);
      
      try 
        (let ast =
           match parseInput() with
           | Some ast' ->
              ast'
           | None ->
              raise Parsing.Parse_error
         in
         let result = Interpret.interpret ast context in
         match result with
         | Interpret.Left (typ, v, ctx') ->
            (
              str_out := String.concat "" [Ast.toString v; ":";
                                           Ast.toString typ; "\n"];
              if not !quiet
              then
                (
                  output_string stdout !str_out;
                  flush stdout;
                );
              interpreter ctx'
            )
         | Interpret.Right s ->
            (
              output_string stdout s;
              flush stdout;
              interpreter context;
            )
	)
      with
      | Parsing.Parse_error ->
         (
           output_string stdout "Error: bad syntax";
           flush stdout;
           interpreter context
         )
      | Exit as ex -> raise ex
    );;

  let rec makeContext x y = (match x, y with
    | [], [] -> []
    | (a, b)::s, (c, d)::t -> (a, (b, Some d))::(makeContext s t)
    | _, _ -> raise (Failure "unable to make context\n"));;

  let ctx = ref (makeContext stBasis dyBasis);;

  let _ =
    try interpreter ctx
    with Exit -> ();;

end;;
