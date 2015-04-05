(*
 * file: symbol.ml
 * author: Bob Muller
 * date: 1-9-2009. 
 * 
 * This module contains the representation of symbols.
 *)
type t = string;;

let format sym = sym;;

let fromString s = s;;

let counter = ref 0;;

let fresh() =
  let freshNumber = !counter
  in
    (
      counter := !counter + 1;
      ("x" ^ (string_of_int freshNumber))
    );;

let compare = String.compare;;
