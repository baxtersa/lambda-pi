module Ast :
sig
  type variable = String of string
		  | Symbol of string * int
		  | Dummy
		      
  type term = Var of variable
	      | Star
	      | Pi of abstraction
	      | Lambda of abstraction
	      | App of term * term
	      | Int of int
	      | Bool of int
	      | Ann of term * term
	      | If of term * term * term
	      | And of term * term
	      | Or of term * term
	      | Op of term * term list
	      | Let of abstraction
	      | IntType
	      | BoolType
	      | List of term * term
	      | Prod of term list
	      | BinaryOp of (term * term -> term)
	      | UnaryOp of (term -> term)
	      | Nil of term
	      | Cons of term * term * term * term
	      | IsNil of term
	      | Head of term
	      | Tail of term
		  
  and abstraction =
    variable * term * term

  val fresh : variable -> variable      
  val subst : (variable * term) list -> term -> term
  val makeString : string -> variable
  val toString : term -> string
end;;
