(* file: prims.mli
 * author: Bob Muller
 * date: Feb. 20, 2009

 * This file contains the names of primitive operators in PEL.
 *)

type t = Plus | Minus | Times | Div | Mod | Exp | Lt | Le | Eq | Ne | Ge | Gt | Not;;

val names : t list;;

val format : t -> string;;

val compare : t -> t -> int;;
