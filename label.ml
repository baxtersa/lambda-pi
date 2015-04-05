(*
 * file: label.ml
 * author: Bob Muller
 * date: Feb. 20, 2009
*)
type label = string

let counter = ref 0

let stringToLabel(x:string) : label = x
      
let fresh() =
  let freshNumber = !counter
  in
    (
      counter := !counter + 1;
      "l" ^ (string_of_int freshNumber)
    )
      
let format x = x
