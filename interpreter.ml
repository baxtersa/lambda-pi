
module type INTERPRETER = 
sig
  open Ast
  open Environment
  val interpreter : Ast.value Environment.t * Ast.value Environment.t -> unit
end;;

module Interpreter : INTERPRETER =
struct
  open Bases;;
  open Staticsemantics;;
  open Dynamicsemantics;;
  open Environment;;
  
  let stBasis = Bases.Interpreter.staticBasis;;
  let dyBasis = Bases.Interpreter.dynamicBasis;;
  
  let debug = ref false;;
  let typeChecking = ref true;;
  
  let parseInput() =
    let inch = input_line stdin in
    let lexbuf = Lexing.from_string inch in
    let ast = Parser.input Lexer.token lexbuf in
    ast;;
  
  let rec interpreter (tenv, venv) : unit =
    (
      output_string stdout ("\nlpi> ");
      flush stdout;
      
      try 
        (let ast = parseInput()
         in (try
               (match Staticsemantics.infer tenv venv ast with
                   (Some typ, tenv') -> let _ = 
                                          if !debug then
                                            (
                                              output_string stdout "Expression is: ";
                                              output_string stdout (Ast.toString ast);
                                              output_string stdout "\n";
                                              flush stdout
                                            )
                                          else () in
                                        (try let (value, venv') = Dynamicsemantics.eval venv ast in
                                             (
                                               output_string stdout ((Ast.toStringValue value) ^ ":" ^ (Ast.toStringValue typ));
                                               output_string stdout "\n";
                                               flush stdout;
                                               interpreter(tenv', venv')
                                             )
                                         with
                                             Failure s -> 
                                               (
                                                 output_string stdout s;
                                                 flush stdout;
                                                 interpreter(tenv, venv)
                                               ))
                 | _ ->
                     (
                       output_string stdout "Input string does not type-check...\n";
                       flush stdout;
                       interpreter(tenv, venv);
                     ))
           with Failure s ->
             (
               output_string stdout s;
               flush stdout;
               interpreter(tenv, venv)
             )
         )
        ) 
      with Parsing.Parse_error ->
        (
          output_string stdout "Input string does not parse...\n";
          flush stdout;
          interpreter(tenv, venv)
        )
        | Assert_failure l ->
            (
              output_string stdout "Inut string does not type-check...\n";
              flush stdout;
              interpreter(tenv, venv)
            )
    );;
  
  let _ = interpreter(stBasis, dyBasis);;
end;;
