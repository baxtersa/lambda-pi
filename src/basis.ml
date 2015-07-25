(*
 * file: bases.ml.
 * 
 * author: Sam Baxter
 * 
 * This file contains the code for building both the static and dynamic basis.
 * The function makeBasis will construct a static basis when applied to the
 * list Bases.primOpTypes.  The same function will construct a dynamic
 * basis when applied to the list implementationsOfPrimitives.   These bases
 * are constructed in the preamble to the top-level read-eval-print loop in
 * Lx.sml.  The apply functions can used to actually put a type checker for (or
 * implementation of) a primitive to use.
 *
 * To extend the basis with new primitives, the list primNames must be
 * extended with an appropriate identifier.
 *)

open Ast;;
open Environment;;

(* 
 * This is the master list of names of primitive operators. 
 * 
 * NB: THESE NAMES ARE LAYED OUT IN A FIXED ORDER!
 *)
let primOpNames = ["+"; "-"; "*"; "/"; "%"; "**"; "<"; "<="; "=="; "<>"; ">"; ">="; "not"];;

let rec zip = function
  | [], [] -> []
  | x::xs, y::ys -> (x, y)::(zip(xs, ys))
  | _ -> raise (Failure "cannot zip lists of unequal length\n")

let makeBasis values =
  let primOpNames' = List.map Ast.makeString primOpNames in
  let keyValuePairs = zip(primOpNames', values) in
  let insert map (key,value) = (key, value)::map
  in
    List.fold_left insert [] keyValuePairs;;

module Interpreter =
  struct
    let applyBinary = function
	(operation,[value1;value2]) -> operation(value1,value2)
      | _ -> raise (Failure "Cannot happen.");;
     
    let applyUnary = function
	(operation,[value]) -> operation(value)
      | _ -> raise (Failure "Cannot happen.");;

      (****************************************************************************
       *
       * The implementation of primitive operations. Note that the order of
       * these together with the unarys must match up with the order of the
       * operator names in op.ml.
       *)
    let intCrossInt2Int =
      Ast.Pi(Ast.makeString "qqqqqqq", (Ast.Prod [Ast.IntType; Ast.IntType]), Ast.IntType);;
      
    let intCrossInt2Bool = 
      Ast.Pi(Ast.makeString "qqqqqqq", (Ast.Prod [Ast.IntType; Ast.IntType]), Ast.BoolType);;

    let bool2Bool = 
      Ast.Pi(Ast.makeString "qqqqqqq", (Ast.Prod [Ast.BoolType]), Ast.BoolType);;

    let operatorTypes = 
      [
	intCrossInt2Int;  (* + *)
	intCrossInt2Int;  (* - *)
	intCrossInt2Int;  (* * *)
	intCrossInt2Int;  (* / *)
	intCrossInt2Int;  (* % *)
	intCrossInt2Int;  (* ** *)
	intCrossInt2Bool; (* < *)
	intCrossInt2Bool; (* <= *)
	intCrossInt2Bool; (* == *)
	intCrossInt2Bool; (* <> *)
	intCrossInt2Bool; (* >= *)
	intCrossInt2Bool; (* > *)
	bool2Bool         (* not *)
      ];;

    let staticBasis = makeBasis operatorTypes;;

    let binaryPrePrimOps = 
      [
        (function
	| (Ast.Int(v1),                (* + *)
           Ast.Int(v2)) ->
          Ast.Int(v1+v2)
	| (a, b) ->
	  Ast.Op(Ast.Var(Ast.String("+")), [a;b]));
	
	(function
	| (Ast.Int(v1),
	   Ast.Int(v2)) ->             (* - *)
	  Ast.Int (v1 - v2)
	| (a, b) ->
	  Ast.Op(Ast.Var(Ast.String("-")), [a;b]));
        
	(fun (Ast.Int(v1),
	      Ast.Int(v2)) ->             (* * *)
	  Ast.Int (v1 * v2));
	
	(fun (Ast.Int(v1),
	      Ast.Int(v2)) ->             (* / *)
	  Ast.Int (v1 / v2));
	
	(fun (Ast.Int(v1),
	      Ast.Int(v2)) ->             (* % *)
	  Ast.Int (v1 mod v2));
	
	(fun (Ast.Int(v1),
	      Ast.Int(v2)) ->             (* ** *)
	  let v1' = float_of_int v1 in
          let v2' = float_of_int v2 in
          Ast.Int(int_of_float(v1' ** v2')));
	
	(fun (Ast.Int(v1),
	      Ast.Int(v2)) ->             (* < *)
	  Ast.Bool(if v1 < v2 then 1 else 0));
	
	(fun (Ast.Int(v1),
	      Ast.Int(v2)) ->             (* <= *)
	  Ast.Bool(if v1 <= v2 then 1 else 0));
	
	(function
	| (Ast.Int(v1),
	   Ast.Int(v2)) ->             (* == *)
	  Ast.Bool(if v1 = v2 then 1 else 0)
	| (a, b) ->
	  Ast.Op(Ast.Var(Ast.String("==")), [a;b]));
	
	(function
	| (Ast.Int(v1),
	   Ast.Int(v2)) ->             (* <> *)
	  Ast.Bool(if v1 <> v2 then 1 else 0)
	| (a, b) ->
	  Ast.Op(Ast.Var(Ast.String("<>")), [a;b]));

	(function
	| (Ast.Int(v1),
	   Ast.Int(v2)) ->             (* >= *)
	  Ast.Bool(if v1 >= v2 then 1 else 0)
	| (a, b) ->
	  Ast.Op(Ast.Var(Ast.String(">=")), [a;b]));

	(function
	| (Ast.Int(v1),
	   Ast.Int(v2)) ->             (* > *)
	  Ast.Bool(if v1 > v2 then 1 else 0)
	| (a, b) ->
	  Ast.Op(Ast.Var(Ast.String(">")), [a;b]));
	
      ];;

      (* 
       * Coerce the implementations of binary primitives to be Ast.letues.
       *)
    let binaryPrimOps = List.map (fun x -> Ast.BinaryOp x) binaryPrePrimOps;;
    
      (*
       *  The unary primtives.
       *)
    let unaryPrePrimOps = 
      [
        (function
	| (Ast.Bool(v)) ->            (* not *)
          Ast.Bool(if v = 1 then 0 else 1)
	| (a) ->
	  Ast.Op(Ast.Var(Ast.String("not")), [a]))
      ];;
    
      (*
       * Coerce the implementations of unary primitives to be Ast.values.
       *)
    let unaryPrimOps = List.map (fun x -> Ast.UnaryOp x) unaryPrePrimOps;;

      (*
       * Make the dynamic basis for export to the interpreter.
       *)
    let dynamicBasis = makeBasis (binaryPrimOps @ unaryPrimOps);;

    end;;
