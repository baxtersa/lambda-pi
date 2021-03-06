val primOpNames : string list
val makeBasis : 'a list -> (Ast.Ast.variable * 'a) list
module Interpreter :
  sig
    val staticBasis : (Ast.Ast.variable * Ast.Ast.term) list
    val dynamicBasis : (Ast.Ast.variable * Ast.Ast.term) list
  end
