(*
 * file: dynamicsemantics.ml
 *
 * Sam Baxter
 * CS397 Honors Thesis
 * Fall '13 - Spring '14
 *
 * This file contains a dynamic semantics for the programming language
 * SFL.  SFL is a strongly and statically typed first order language with
 * integer and boolean datatypes.
 *)

module type DYNAMICSEMANTICS = 
sig
  open Ast
  open Environment
  val eval : Ast.value Environment.t -> Ast.term -> Ast.value * Ast.value Environment.t
  val refine : Ast.value Environment.t -> Ast.value -> Ast.value
end
  
module Dynamicsemantics : DYNAMICSEMANTICS = 
struct
  open Ast;;
  open Symbol;;
  open Bases;;
  open Environment;;

  let rec eval env = function
  Ast.Id(symb) -> (try (Environment.find symb env, env) with
      Not_found -> (*let id = Symbol.format symb in
                   raise (Failure ("dynamicsemantics:  symbol " ^ id ^ " not found\n")))*)
    (Ast.VId(symb), env))
    | Ast.Abs(symb, ty, e) -> 
        let tyval = fst (eval env ty) in
        (Ast.AbsVal(symb, tyval, e, Ast.Env(env)), env) 
    | Ast.App(Ast.Id e1, e2) -> (let args = List.map fst (List.map (eval env) e2) in
                                 (match Environment.find e1 env with
                                     Ast.BinaryOp oper -> (Bases.Interpreter.applyBinary(oper, args), env)
                                   | Ast.UnaryOp oper -> (Bases.Interpreter.applyUnary(oper, args), env)))
    | Ast.CApp(e, e') -> 
        let rator = fst (eval env e) in
        (match rator with
            Ast.AbsVal(symb, typ, body, env') ->
              let rand = fst (eval env e') in
              let env'' = Environment.add symb rand env in
              (fst (eval env'' body), env)
          | _ -> (Ast.TInt, env))
        
    | Ast.If(e1, e2, e3) -> (match eval env e1 with
        (Ast.VBool(1), _) -> eval env e2
        | (Ast.VBool(0), _) -> eval env e3)
    | Ast.And(e1, e2) -> (let (e1val, not_used) = eval env e1 in
                      (match e1val with
                          Ast.VBool(1) -> eval env e2
                        | Ast.VBool(0) -> (e1val, not_used)))
    | Ast.Or(e1, e2) -> (let (e1val, not_used) = eval env e1 in
                     (match e1val with
                         Ast.VBool(1) -> (e1val, not_used)
                       | Ast.VBool(0) -> eval env e2))
    | Ast.Int(i) -> (Ast.VInt(i), env)
    | Ast.IntType -> (Ast.TInt, env)
    | Ast.Bool(i) -> (Ast.VBool(i), env)
    | Ast.BoolType -> (Ast.TBool, env)
    | Ast.DepList(len, typ) -> let tlen = fst (eval env len) in
                               let typ' = fst (eval env typ) in
                               (Ast.TList(tlen, typ'), env)
    | Ast.Nil(typ) -> let typ' = fst (eval env typ) in
                      (Ast.VNil(typ'), env)
    | Ast.Cons(len, typ, hd, tl) ->
        let vlen = fst (eval env len) in
        let vtyp = fst (eval env typ) in
        let vhd = fst (eval env hd) in
        let vtl = fst (eval env tl) in
        (Ast.VCons(vlen, vtyp, vhd, vtl), env)
    | Ast.IsNil(term) -> let v = fst (eval env term) in
                         (match v with
                             Ast.VNil(_) -> (Ast.VBool(1), env)
                           | _ -> (Ast.VBool(0), env))
    | Ast.Head(term) -> let v = fst (eval env term) in
                        (match v with
                            Ast.VNil(_) -> raise (Failure "dynamicsemantics: cannot retrieve head of an empty list\n")
                          | Ast.VCons(len, typ, hd, tl) -> (hd, env)
                          | _ -> raise (Failure "dynamicsemantics: head requires input of type list\n"))
    | Ast.Tail(term) -> let v = fst (eval env term) in
                        (match v with
                            Ast.VNil(t) as value -> (value, env)
                          | Ast.VCons(len, typ, hd, tl) -> (tl, env)
                          | _ -> raise (Failure "dynamicsemantics: tail requires input of type list\n"))
    | Ast.Star -> (Ast.VStar, env)
    | Ast.Ann(id, t) -> eval env id
    | Ast.Fun(Ast.Ann(Ast.Id(id), e), e') -> 
        let t = fst (eval env e) in
        let t' = fst (eval env e') in
        (Ast.VFun(id, t, t'), env)
                
  and makeFormalArgsEnv env formals args =
    (match formals with
        [] -> env
      | (name, typ)::more -> (match args with
          [] -> env
          | a::moreargs -> (let env' = Environment.add name a env in
                            makeFormalArgsEnv env' more moreargs)))
      
  and refine venv = function
  Ast.TInt as t -> t
    | Ast.TBool as t -> t
    | Ast.VStar as t -> t
    | Ast.TList(len, typ) -> Ast.TList(refine venv len, refine venv typ)
    | Ast.AbsVal(symb, typ, body, env) -> Ast.AbsVal(symb, refine venv typ, body, env)
    | Ast.VFun(id, domain, range) -> 
        Ast.VFun(id, refine venv domain, refine venv range)
    | Ast.VId(id) as t -> try (Environment.find id venv) with
        Not_found -> t
  
end;;
