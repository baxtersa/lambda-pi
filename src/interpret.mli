module Interpret :
  sig
    type result = Ast.Ast.term * Ast.Ast.term * Environment.context ref
    type 'a either = Left of 'a | Right of string

    val ast_from_string : string -> Ast.Ast.term option
    val interpret : Ast.Ast.term -> Environment.context ref -> result either
  end;;
