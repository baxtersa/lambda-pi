module type STATICSEMANTICS = 
sig
  open Ast
  open Environment
  open Dynamicsemantics
  val infer : Ast.value Environment.t -> Ast.value Environment.t -> Ast.term -> Ast.value option * Ast.value Environment.t
  val check : Ast.value Environment.t -> Ast.value Environment.t -> Ast.value -> Ast.term -> bool 
end;;

module Staticsemantics : STATICSEMANTICS = 
struct
  open Ast;;
  open Symbol;;
  open Bases;;
  open Environment;;
  open Dynamicsemantics;;
  
  let processArgTypes(typs) =
    if List.exists(function None->true | _ ->false) typs then
      None
    else
      (match List.map (function (Some t) -> t | None -> Ast.TInt) typs with
          [t] -> Some t
        | ts -> Some (Ast.VProd(ts)));;

  let rec check tenv venv typ = function
  Ast.Abs(symb, typ', body) ->
    (match typ with
        VFun(id, domain, range) ->
          let tenv' = Environment.add symb domain tenv in
          check tenv' venv range body
      | _ -> false)
      

    | term -> let Some typ' = fst (infer tenv venv term) in
              Ast.compare typ typ' = 0

    | _ -> raise (Failure "staticsemantics:  not a checkable term\n")
        
  and
      infer tenv venv = function
      Ast.Star -> (Some Ast.VStar, tenv)
       
        | Ast.IntType -> (Some Ast.VStar, tenv)

        | Ast.BoolType -> (Some Ast.VStar, tenv)

        | Ast.Int(i) -> (Some Ast.TInt, tenv)

        | Ast.Bool(i) -> (Some Ast.TBool, tenv)

        | Ann(e,ro) ->
            (
              assert (check tenv venv Ast.VStar ro);
              let typ = fst (Dynamicsemantics.eval venv ro) in
              assert (check tenv venv typ e);
              (Some typ, tenv)
            )
              
        | Ast.Star -> (Some Ast.VStar, tenv)
            
        | Ast.Fun(Ast.Ann(Ast.Id(id), e), e') -> 
            assert (check tenv venv Ast.VStar e);
            let typ = fst (Dynamicsemantics.eval venv e) in
            let venv' = Environment.add id typ venv in
            let tenv' = Environment.add id typ tenv in
            assert (check tenv' venv' Ast.VStar e');
            (Some Ast.VStar, tenv)
              
        | Ast.Id(id) ->
            (try (Some (Environment.find id tenv), tenv) with
                Not_found -> (raise (Failure "staticsemantics:  symbol not found\n")))

        | Ast.App(Ast.Id rator, rands) ->
            let argTypeOptions = List.map fst (List.map (infer tenv venv) rands) in
            (match processArgTypes argTypeOptions with
                None -> raise (Failure "staticsemantics:  Input string does not type-check...\n")
              | Some argTypes ->
                  let typ = (try Some (Environment.find rator tenv) with
                      Not_found -> raise (Failure "staticsemantics:  operator not found")) in
                  (match typ with
                      Some Ast.VFun(id, domain, range) ->
                        if Ast.compare domain argTypes = 0 then
                          (Some range, tenv)
                        else
                          raise (Failure "staticsemantics:  Input string does not type-check...\n")
                    | _ -> raise (Failure "staticsemantics:  input string does not type-check...\n")))
        
        | Ast.CApp(e, e') ->
            let Some typ = fst (infer tenv venv e) in
            (match typ with
                Ast.VFun(id, t, t') as funtyp ->
                  assert (check tenv venv t e');
                  let bodyval = fst (Dynamicsemantics.eval venv e') in
                  let venv' = Environment.add id bodyval venv in
                  (Some (Dynamicsemantics.refine venv' t'), tenv))

        | _ -> raise (Failure "staticsemantics:  not an inferable term\n")

  and subst_up i r = function
    | Ast.Ann(e, tao) ->
        Ast.Ann(subst_down i r e, subst_down i r tao)
    | Ast.Bound j ->
        if i = j then r else Ast.Bound j
    | Ast.Free y -> 
        Ast.Free y
    | Ast.App(e, e') -> 
        Ast.App(subst_up i r e, subst_down i r e')
    | Ast.Star as x->
        x
    | Ast.Pi(tao, tao') ->
        Ast.Pi(subst_down i r tao, subst_down (i + 1) r tao')

  and subst_down i r = function
    | Ast.Inf e -> 
        Ast.Inf(subst_up i r e)
    | Ast.Lam e -> 
        Ast.Lam(subst_down (i + 1) r e);;


end;;
