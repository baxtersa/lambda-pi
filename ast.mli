(* file: ast.mli
   author: Bob Muller
*)
open Environment;;

type term = Id of Symbol.t
            | Abs of Symbol.t * term * term
            | App of term * term list
            | CApp of term * term
            | If of term * term * term
            | And of term * term
            | Or of term * term
            | Bind of declaration
            | Let of declaration * term
            | Int of int
            | IntType
            | Bool of int
            | BoolType
            | Star
            | DepList of term * term
            | Nil of term
            | Cons of term * term * term * term
            | IsNil of term
            | Head of term
            | Tail of term
            | Ann of term * term
            | Fun of term * term
and
  declaration = ValBind of Symbol.t * term * term 

and
  value = VId of Symbol.t
          | BinaryOp of (value * value -> value)
          | UnaryOp of (value -> value)
          | AbsVal of Symbol.t * value * term * environment
          | VInt of int
          | VBool of int
          | TInt
          | TBool
          | TList of value * value
          | VNil of value
          | VCons of value * value * value * value
          | VStar
          | VProd of value list
          | VFun of Symbol.t * value * value

and
  environment = Env of value Environment.t;;

val toString : term -> string;;
val toStringValue : value -> string;;

val compare : value -> value -> int
