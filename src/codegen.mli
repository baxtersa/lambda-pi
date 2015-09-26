module Codegen :
sig
  val translate : Ast.term -> Codestream.codestream
end;;
