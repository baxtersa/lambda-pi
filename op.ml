(* file: prims.ml
 * author: Bob Muller
 * date: Feb. 20, 2009
 * 
 * This file contains the names of primitive operators in PEL
 *)

type t = Plus | Minus | Times | Div | Mod | Exp | Lt | Le | Eq | Ne | Ge | Gt | Not;;

let names = [Plus; Minus; Times; Div; Mod; Exp; Lt; Le; Eq; Ne; Ge; Gt; Not];;

let format = function 
    Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Exp -> "**"
  | Lt -> "<"
  | Le -> "<="
  | Eq -> "=="
  | Ne -> "<>"
  | Ge -> ">="
  | Gt -> ">"
  | Not -> "not";;

let compare o1 o2 = String.compare (format o1) (format o2);;
