(*
* file: arm.mli
* author: Sam Baxter
* date: 1 August 2015
*)

module Operand :
sig
  type register

  type operand = 
  | Const8 of int
  | Const16 of int
  | Const32 of int
  | Label of string
  | String of string
  | Register of register
  | Indirect of int option * register

  val toString : operand -> string
end;;

module Operation :
sig
  type operation =
  | Add of operand * operand * operand
  | Sub of operand * operand * operand
  | Cmp of operand * operand
  | Mov of operand * operand * operand
  | Ldr of operand * operand * operand
  | Str of operand * operand * operand

  val toString : operation -> string
end;;

module Instruction :
sig
  type instruction

  val toInstruction : string option -> operation -> instruction
  val instructionLabel : instruction -> string option
  val instructionOperation : instruction -> operation

  val toString : instruction -> string
end;;

module Codestream :
sig
  type codestream

  val emit : string -> codestream -> unit
end;;
