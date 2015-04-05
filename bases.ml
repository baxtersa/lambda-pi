(*
 * file: bases.ml.
 * author: R. Muller
 * date: 4-4-2002, converted from SML to Ocaml on 1-8-2009.
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

open Environment;;

(* 
 * This is the master list of names of primitive operators. 
 * 
 * NB: THESE NAMES ARE LAYED OUT IN A FIXED ORDER!
 *)
let primOpNames = ["+"; "-"; "*"; "/"; "%"; "**"; "<"; "<="; "=="; "<>"; ">"; ">="; "not"];;

let makeBasis values =
  let primOpNames' = List.map Symbol.fromString primOpNames in
  let keyValuePairs = Util.zip(primOpNames', values) in
  let insert map (key,value) = Environment.add key value map
  in
    List.fold_left insert Environment.empty keyValuePairs;;

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
      Ast.VFun(Symbol.fromString "x", (Ast.VProd [Ast.TInt; Ast.TInt]), Ast.TInt);;
    (*Ast.VFun(Ast.TInt, Ast.VProd [Ast.TInt; Ast.TInt], Ast.TInt);;*)
      
    let intCrossInt2Bool = 
      Ast.VFun(Symbol.fromString "x", (Ast.VProd [Ast.TInt; Ast.TInt]), Ast.TBool);;
    (*Ast.VFun(Ast.TInt, Ast.VProd [Ast.TInt; Ast.TInt], Ast.TBool);;*)

    let int2Int = 
      Ast.VFun(Symbol.fromString "x", (Ast.VProd [Ast.TInt]), Ast.TInt);;
    (*Ast.VFun(Ast.TInt, Ast.TInt, Ast.TInt);;*)

    let bool2Bool = 
      Ast.VFun(Symbol.fromString "x", Ast.TBool, Ast.TBool);;
    (*Ast.VFun(Ast.TInt, Ast.TBool, Ast.TBool);;*)

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
        (fun (Ast.VInt(v1),                (* + *)
              Ast.VInt(v2)) ->
          Ast.VInt(v1+v2));

	(fun (Ast.VInt(v1),
	      Ast.VInt(v2)) ->             (* - *)
	  Ast.VInt (v1 - v2));
        
	(fun (Ast.VInt(v1),
	      Ast.VInt(v2)) ->             (* * *)
	  Ast.VInt (v1 * v2));

	(fun (Ast.VInt(v1),
	      Ast.VInt(v2)) ->             (* / *)
	  Ast.VInt (v1 / v2));

	(fun (Ast.VInt(v1),
	      Ast.VInt(v2)) ->             (* % *)
	  Ast.VInt (v1 mod v2));

	(fun (Ast.VInt(v1),
	      Ast.VInt(v2)) ->             (* ** *)
	  let v1' = float_of_int v1 in
          let v2' = float_of_int v2 in
          Ast.VInt(int_of_float(v1' ** v2')));

	(fun (Ast.VInt(v1),
	      Ast.VInt(v2)) ->             (* < *)
	  Ast.VBool(if v1 < v2 then 1 else 0));

	(fun (Ast.VInt(v1),
	      Ast.VInt(v2)) ->             (* <= *)
	  Ast.VBool(if v1 <= v2 then 1 else 0));

	(fun (Ast.VInt(v1),
	      Ast.VInt(v2)) ->             (* == *)
	  Ast.VBool(if v1 = v2 then 1 else 0));

	(fun (Ast.VInt(v1),
	      Ast.VInt(v2)) ->             (* <> *)
	  Ast.VBool(if v1 <> v2 then 1 else 0));

	(fun (Ast.VInt(v1),
	      Ast.VInt(v2)) ->             (* >= *)
	  Ast.VBool(if v1 >= v2 then 1 else 0));

	(fun (Ast.VInt(v1),
	      Ast.VInt(v2)) ->             (* > *)
	  Ast.VBool(if v1 > v2 then 1 else 0));

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
          (fun (Ast.VBool(v)) ->            (* not *)
            Ast.VBool(if v = 1 then 0 else 1))
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
