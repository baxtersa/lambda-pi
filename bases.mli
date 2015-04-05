(* file: bases.mli
 * author: Bob Muller
 * date: 3-2-2009
 * 
 * This is the interface file for the bases used in the PEL compiler.
 *)
open Environment

(* The interpreter doesn't include a type checker. Its dynamicBasis provides
 * interpretations of the primitive operators.
 *)
module Interpreter :
sig
  val staticBasis : Ast.value Environment.t
  val dynamicBasis : Ast.value Environment.t
    
  val applyBinary : ('a * 'a -> 'a) * 'a list -> 'a
  val applyUnary  : ('a -> 'a) * 'a list -> 'a
end;;
