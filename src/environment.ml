(*
 * file: environment.ml
 *
 * author: Sam Baxter
 *
 * This file contains the environment code for DepuTy
 *)
open Ast;;

type context = (Ast.variable * (Ast.term * Ast.term option)) list

let lookup_typ x ctx = fst (List.assoc x ctx)

let lookup_value x ctx = snd (List.assoc x ctx)

let extend x t ?value ctx = ctx := (x, (t, value)) :: !ctx


