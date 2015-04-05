(*
 * file: ast.ml
 * author: Sam Baxter
 * 
 * This file lays out the abstract syntax of the language.
 * A substitution function is implemented to handle
 * substitutions of types and values in type checking
 * and the evaluation process.
 * A toString function is provided for the interactive display.
 *)

type variable =
| String of string
| Symbol of string * int
| Dummy
    
type term =
| Var of variable
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
    
let fresh =
  let k = ref 0 in
  function
  | String x | Symbol(x, _) ->
    incr k;
    Symbol(x, !k)
  | Dummy ->
    incr k;
    Symbol("", !k)
      
let rec subst s = function
  | Var x ->
    (try List.assoc x s with Not_found -> Var x)
  | Star ->
    Star
  | Pi a ->
    Pi (subst_abs s a)
  | Lambda a ->
    Lambda (subst_abs s a)
  | App(e1, e2) ->
    App(subst s e1, subst s e2)
  | Int i ->
    Int i
  | Bool b ->
    Bool b
  | Ann(e1, e2) ->
    Ann(subst s e1, subst s e2)
  | If(e1, e2, e3) ->
    If(subst s e1, subst s e2, subst s e3)
  | And(e1, e2) ->
    And(subst s e1, subst s e2)
  | Or(e1, e2) ->
    Or(subst s e1, subst s e2)
  | Op(rator, rands) ->
    Op(rator, List.map (subst s) rands)
  | Let(x, typ, e) ->
    Let(x, subst s typ, subst s e)
  | IntType ->
    IntType
  | BoolType ->
    BoolType
  | List(typ, len) ->
    List(subst s typ, subst s len)
  | Nil e -> Nil (subst s e)
  | Cons(len, typ, el, rest) ->
    Cons(subst s len, subst s typ, subst s el, subst s rest)
  | IsNil e ->
    IsNil(subst s e)
  | Head e ->
    Head (subst s e)
  | Tail e ->
    Tail (subst s e)

and subst_abs s (x, t, e) =
  let x' = fresh x in
  (x', subst s t, subst ((x, Var x')::s) e)

let makeString s = String s

let rec toString = function
  | Var x -> toStringVar x
  | Star -> "~"
  | Pi a -> "Pi" ^ (toStringAbs a)
  | Lambda a -> "Lambda" ^ (toStringAbs a)
  | App(e, e') -> "App(" ^ (toString e) ^ ", " ^ (toString e') ^ ")"
  | Int i -> string_of_int i
  | Bool i -> if i = 0 then "false" else "true"
  | Ann(e, t) -> toString e
  | If(e1, e2, e3) ->
    "if " ^ (toString e1) ^ " then " ^ (toString e2) ^ " else " ^ (toString e3)
  | And(e1, e2) ->
    "&(" ^ (toString e1) ^ ", " ^ (toString e2) ^ ")"
  | Or(e1, e2) ->
    "||(" ^ (toString e1) ^ ", " ^ (toString e2) ^ ")"
  | Op(rator, rands) -> (toString rator) ^ "(" ^ (toStringList rands) ^ ")"
  | Let a -> "Let" ^ (toStringAbs a)
  | IntType -> "int"
  | BoolType -> "bool"
  | List(typ, len) -> (toString typ) ^ " list(" ^ (toString len) ^ ")"
  | Prod x -> toStringTuple x
  | BinaryOp fn -> "bi-op"
  | UnaryOp fn -> "u-op"
  | Nil e -> "[]:" ^ (toString e)
  | Cons(len, typ, el, rest) -> "more[" ^ (toString len) ^ " ," ^ (toString typ) ^ "] " ^ (toString el) ^ " " ^ (toString rest)
  | IsNil e -> "is_nil(" ^ (toString e) ^ ")"
  | Head e -> "head(" ^ (toString e) ^ ")"
  | Tail e -> "tail(" ^ (toString e) ^ ")"

and toStringTuple = function
  | [] -> ""
  | [a] -> toString a
  | x::xs -> (toString x) ^ " * " ^ (toStringTuple xs)

and toStringList = function
  | [] -> ""
  | [only] -> toString only
  | x::xs -> (toString x) ^ ", " ^ (toStringList xs)

and toStringVar = function
  | String s -> s
  | Symbol(s, i) -> s

and toStringAbs(x, t, e) = "(" ^ (toStringVar x) ^ ", " ^ (toString t) ^ ", " ^ (toString e) ^ ")"
