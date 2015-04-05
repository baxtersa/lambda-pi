(* file: symbol.mli
   author: Bob Muller
   date: February 20, 2009

   This is the interface file for symbols.
*)
type t

val format : t -> string
val fromString : string -> t
val fresh : unit -> t
val compare : t -> t -> int
