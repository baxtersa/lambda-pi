(*
  file: ast.ml
  author: Bob Muller
  date: January 4, 2009

  This file contains an abstract syntax for the programming language PEL.
  The abstract syntax includes a few items that aren't used directly in
  PEL but (some of which) are used in transformations on PEL programs.
 *)

module Environment = Environment.Environment;;
module Sym = Symbol;;
open Environment;;
open Symbol;;

type term = Id of Sym.t
            | Abs of Sym.t * term * term
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
            | Head of term | Tail of term
            | Ann of term * term
            | Fun of term * term

and
  declaration = ValBind of Sym.t * term * term 

and
  value = VId of Sym.t
          | BinaryOp of (value * value -> value)
          | UnaryOp of (value -> value)
          | AbsVal of Sym.t * value * term * environment
          | VInt of int
          | VBool of int
          | TInt
          | TBool
          | TList of value * value
          | VNil of value
          | VCons of value * value * value * value
          | VStar
          | VProd of value list
          | VFun of Sym.t * value * value

and
  environment = Env of value Environment.t;;

(***********************************************************************************
										   *
     * toString utilities.
     *
     * The following is a utility function for making a list of comma-separated
     * strings where the items separated are arbitrary.
*)
let rec toStringList = function
([], _) -> ""
  | ([only], toStringer) -> toStringer only
  | (first::rest, toStringer) -> (toStringer first) ^ ", " ^ toStringList(rest,toStringer)
      
let rec toStringTuple = function
(first::rest, toStringer) -> (toStringer first) ^ " * " ^ toStringTuple(rest, toStringer)
  
(* 
 * make a string for a binding occurrence: x:t
 *)
let toStringBindOcc name typ toStringer = Sym.format name ^ " : " ^ (toStringer typ)
  
(* 
 * make a string for an assignment: M = N
 *)
let toStringEqual(s1,s2) = s1 ^ " = " ^ s2
  
(*
 * The toString function on terms.
 *)
let rec toString = function
(Id name) -> Sym.format name
  
  | App(Id rator,rand) ->
      let ras = Sym.format rator in
      let commaSepStringList = toStringList(rand,toString)
      in
      ras ^ "(" ^ commaSepStringList ^ ")"
        
  | App(anythingelse,_) -> raise (Failure "Ast: bad application, can't happen.")
      
  | If(test,thenpart,elsepart) ->
      let p1 = toString test in
      let p2 = toString thenpart in
      let p3 = toString elsepart
      in
      "If(" ^ p1 ^ ", " ^ p2 ^ ", " ^ p3 ^ ")"
        
  | Or(left,right) ->
      let p1 = toString left in
      let p2 = toString right 
      in
      "(" ^ p1 ^ " or " ^ p2 ^ ")"
        
  | And(left,right) ->
      let p1 = toString left in
      let p2 = toString right
      in
      "(" ^ p1 ^ " and " ^ p2 ^ ")"
        
  | Let(defn,body) ->
      let ds = toStringDec(defn) in
      let bs = toString body
      in
      "Let(" ^ ds ^ ", " ^ bs ^ ")"
  | Bind(defn) ->
      let ds = toStringDec(defn)
      in
      "Bind(" ^ ds ^ ")"
  | Int(i) ->
      string_of_int i
  | Bool(i) ->
      if i = 0 then "false" else "true"
  | IntType ->
      "int"
  | BoolType ->
      "bool"
  | Star ->
      "~"
  | DepList(len, t) -> 
      (toString t) ^ " list"
  | Cons(len, t, hd, tl) -> 
      (toString t) ^ " list"
  | Nil(typ) -> 
      "List(0):" ^ (toString typ)
  | Ann(id, t) -> let name = toString id in
                  name ^ ":" ^ (toString t)
  | Fun(t1, t2) ->
      (toString t1) ^ "->" ^ (toString t2) 
        
and toStringDec = function
ValBind(id,typ,definition) -> toStringEqual(toStringBindOcc id typ toString, toString definition)
  
(*
 * toStringValue : value -> string
 *)
and toStringValue = function
AbsVal(_,_,_,_) -> "Closure"

  | VId(id) -> Sym.format id  

  | VInt(i) -> (string_of_int i)
      
  | VBool(i) -> let v = if i = 0 then "false" else "true" in v

  | TInt -> "int"

  | TBool -> "bool"
      
  | VStar -> "~"
      
  | VProd(vlist) -> "(" ^ toStringTuple(vlist, toStringValue) ^ ")"
      
  | VNil(typ) -> "stop[" ^ (toStringValue typ) ^ "]"
  | VCons(len, typ, hd, tl) ->
      "more[" ^ (toStringValue typ) ^ "] " ^ (toStringValue hd) ^ " " ^ (toStringValue tl)

  | TList(len, v) -> let typ = toStringValue v in
                     typ ^ " list(" ^ (toStringValue len) ^ ")"

  | VFun(id, v1, v2) -> "pi " ^ (Sym.format id) ^ ":" ^ (toStringValue v1) ^ "." ^ (toStringValue v2)
                      
  | _ -> "Unprintable environment value";;


let rec preOrder = function
VStar -> [0]
  | TBool -> [1]
  | TInt -> [2]
  | VFun(id, from, too) -> [3] @ preOrder(from) @ preOrder(too)
  | VProd(l) -> 4 :: List.fold_right (@) (List.map preOrder l) []
  | VId(id) -> [7]
  | TList(len, typ) -> match len with
      VInt(i) -> 5 :: i :: preOrder(typ)
      | TInt -> 6 :: preOrder(typ)

let rec lexicographicOrder = function
([], []) -> 0
  | ([], _) -> -1
  | (_, []) -> 1
  | (n1::rest1, n2::rest2) ->
      (match (Pervasives.compare n1 n2) with
          0 -> lexicographicOrder(rest1, rest2)
        | other -> other);;

let compare t t' = lexicographicOrder(preOrder(t),preOrder(t'));;
