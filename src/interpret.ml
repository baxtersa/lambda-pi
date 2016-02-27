module Interpret =
  struct
    open Ast
    open Basis
    open Staticsemantics
    open Environment

    type result = Ast.term * Ast.term * Environment.context ref
    type 'a either = Left of 'a | Right of string
                    
    let stBasis = Basis.Interpreter.staticBasis
    let dyBasis = Basis.Interpreter.dynamicBasis

    let interpret ast ctx =
      try
        (
          let typ = Staticsemantics.infer ctx ast in
          let v, ctx' = Staticsemantics.normalize ctx ast in
          Left (typ, v, ctx')
        )
      with Failure s ->
        Right s
  end;;
             
