(*
 * file: staticsemantics.ml
 * 
 * author: Sam Baxter
 * 
 * This file contains a normalize function, which in effect acts as
 * an evaluation function on the abstract syntax.  The infer function
 * infers types of terms and checks their validity according to
 * the formal rules of the dependent type system.  The equal funciton
 * compares the equality of terms/types
 *)
open Ast
open Environment

let rec normalize env = function
  | Ast.Var x ->
    (match
	(try lookup_value x !env
	 with Not_found -> raise (Failure "unknown identifier\n"))
     with
     | None -> (Ast.Var x, env)
     | Some e -> (fst (normalize env e), env))
  | Ast.Star ->
    (Ast.Star, env)
  | Ast.Pi a ->
    (Ast.Pi (normalize_abs env a), env)
  | Ast.Lambda a ->
    (Ast.Lambda (normalize_abs env a), env)
  | Ast.App(e1, e2) ->
    let e2' = fst (normalize env e2) in
    (match fst (normalize env e1) with
    | Ast.Lambda (x, _, e1') ->
      (fst (normalize env (Ast.subst [(x, e2')] e1')), env)
    | e1 ->
      (Ast.App(e1, e2)), env)
  | Ast.Int i ->
    (Ast.Int i, env)
  | Ast.Bool b ->
    (Ast.Bool b, env)
  | Ast.Ann(e1, e2) ->
    (Ast.Ann (fst (normalize env e1), fst (normalize env e2)), env)
  | Ast.If(e1, e2, e3) ->
    (match fst (normalize env e1) with
    | Ast.Bool 1 -> normalize env e2
    | Ast.Bool 0 -> normalize env e3
    | _ -> raise (Failure "test condition of IF statement must normalize to a bool\n"))
  | Ast.And(e1, e2) ->
    (match fst (normalize env e1) with
    | Ast.Bool 0 as t -> (t, env)
    | Ast.Bool 1 -> normalize env e2
    | _ -> raise (Failure "cannot AND non-boolean types\n"))
  | Ast.Or(e1, e2) ->
    (match fst (normalize env e1) with
    | Ast.Bool 1 as t -> (t, env)
    | Ast.Bool 0 -> normalize env e2
    | _ -> raise (Failure "cannot OR non-boolean types\n"))
  | Ast.Op(rator, rands) ->
    let rator' = fst (normalize env rator) in
    (match rator', rands with
    | Ast.BinaryOp f, [a;b] ->
      (f (fst (normalize env a), fst (normalize env b)), env)
    | Ast.UnaryOp f, [a] ->
      (f (fst (normalize env a))), env
    | _ -> raise (Failure "invalid operator symbol\n"))
  | Ast.Prod x ->
    (Ast.Prod (List.map fst (List.map (normalize env) x)), env)
  | Ast.Let (x, t, e) ->
    let t' = fst (normalize env t) in
    let e' = fst (normalize env e) in
    extend x t' ~value:e' env;
    (Ast.Let (x, t', e'), env)
  | Ast.IntType ->
    (Ast.IntType, env)
  | Ast.BoolType ->
    (Ast.BoolType, env)
  | Ast.BinaryOp f as x -> (x, env)
  | Ast.UnaryOp f as x -> (x, env)
  | Ast.List(typ, len) ->
    (Ast.List (fst (normalize env typ), fst (normalize env len)), env)
  | Ast.Nil e -> (Ast.Nil (fst (normalize env e)), env)
  | Ast.Cons(len, typ, el, rest) ->
    (Ast.Cons (fst (normalize env len), fst (normalize env typ), fst (normalize env el), fst (normalize env rest)), env)
  | Ast.IsNil e ->
    (match fst (normalize env e) with
    | Ast.Nil a -> (Ast.Bool 1, env)
    | Ast.Cons(_,_,_,_) -> (Ast.Bool 0, env)
    | _ -> raise (Failure "Input cannot normalize\n"))
  | Ast.Head e ->
    (match fst (normalize env e) with
    | Ast.Cons(_,_,e,_) -> (e, env)
    | _ -> raise (Failure "Cannot normalize head of anything other than non-empty list\n"))
  | Ast.Tail e ->
    (match fst (normalize env e) with
    | Ast.Cons(_,_,_,e) -> (e, env)
    | Ast.Nil e -> (Ast.Nil e, env)
    | _ -> raise (Failure "Cannot normalize tail of anything other than a list\n"))

and normalize_abs env (x, t, e) =
  let t' = fst (normalize env t) in
  (x, t', e)

let rec all_true l =
  (match l with
  | [] -> true
  | [x] -> x
  | x::xs -> x && all_true xs)

let rec apply_list fs ls =
  (match fs, ls with
  | [], [] -> []
  | [f], [x] -> [f x]
  | x::xs, y::ys -> (x y)::(apply_list xs ys)
  | _ -> raise (Failure "Cannot dot apply list of functions to list of operands"))

let equal env e1 e2 =
  let rec equal' e1 e2 =
    (match e1, e2 with
    | Ast.Var x1, Ast.Var x2 -> x1 = x2
    | Ast.App(d1, d2), Ast.App(f1, f2) -> equal' d1 f1 && equal' d2 f2
    | Ast.Star, Ast.Star -> true
    | Ast.Pi a1, Ast.Pi a2 -> equal_abs a1 a2
    | Ast.Lambda a1, Ast.Lambda a2 -> equal_abs a1 a2
    | Ast.Int i, Ast.Int j -> i = j
    | Ast.Bool b, Ast.Bool b' -> b = b'
    | Ast.Ann(d1, d2), Ast.Ann(f1, f2) ->
      equal' d1 f1 && equal' d2 f2
    | Ast.Op(r, rands), Ast.Op(r', rands') ->
      equal' r r' && all_true (apply_list (List.map equal' rands) rands')
    | Ast.Let a1, Ast.Let a2 ->
      equal_abs a1 a2
    | Ast.IntType, Ast.IntType -> true
    | Ast.BoolType, Ast.BoolType -> true
    | Ast.Prod a, Ast.Prod b ->
      (match a, b with
      | [], [] -> true
      | [x], [y] -> equal' x y
      | x::xs, y::ys -> equal' (Ast.Prod xs) (Ast.Prod ys)
      | _ -> false)
    | Ast.List(a, b), Ast.List(x, y) ->
      equal' a x && equal' b y
    | Ast.Nil a, Ast.Nil b ->
      equal' a b
    | Ast.Cons(a1, b1, c1, d1), Ast.Cons(a2, b2, c2, d2) ->
      equal' a1 a2 && equal' b1 b2 && equal' c1 c2 && equal' d1 d1
    | Ast.IsNil a, Ast.IsNil b ->
      equal' a b
    | Ast.Head a, Ast.Head b ->
      equal' a b
    | Ast.Tail a, Ast.Tail b ->
      equal' a b
    | _, _ -> false)

  and equal_abs (x, t, e1) (x', t', e2) =
    let z = Ast.Var (Ast.fresh x) in
    equal' t t' && (equal' (Ast.subst [(x, z)] e1) (Ast.subst [(x', z)] e2))
  in
  equal' (fst (normalize env e1)) (fst (normalize env e2))

let rec infer env = function
  | Ast.Var x ->
    (try lookup_typ x !env
     with Not_found -> raise (Failure "unknown identifier \n"))
  | Ast.Star -> Ast.Star
  | Ast.Pi (x, t, e) ->
    let t' = infer env t in
    let temp = !env in
    extend x t env;
    let e' = infer env e in
    env := temp;
    (match t', e' with
    | Ast.Star, Ast.Star -> Ast.Star
    | _, _ -> raise (Failure "Invalid type in dependent function space \n"))
  | Ast.Lambda (x, t, e) ->
    let temp = !env in
    extend x t env;
    let e' =
      (try infer env e
       with Failure s ->
	 env := temp;
	 raise (Failure ("Input does not type-check\n" ^ s)))
    in
    env := temp;
    Ast.Pi (x, t, e')
  | Ast.App(e1, e2) ->
    let (x, s, t) = infer_pi env e1 in
    let e2' = infer env e2 in
    check_equal env s e2';
    Ast.subst [(x, e2)] t
  | Ast.Int i -> Ast.IntType
  | Ast.Bool b -> Ast.BoolType
  | Ast.Ann(e1, e2) ->
    let t = infer env e1 in
    check_equal env t e2;
    t
  | Ast.If(e1, e2, e3) ->
    (try check_equal env (infer env e1) Ast.BoolType;
	 check_equal env (infer env e2) (infer env e3);
	 infer env e2
     with Failure s ->
       check_equal env (infer env e1) Ast.BoolType;
       (match (infer env e2), (infer env e3) with
       | Ast.List(t, a), Ast.List(t', b) ->
	 check_equal env t t';
	 Ast.List(t', b)
       | _, _ -> raise (Failure ("Ast.If statement does not type-check\n" ^ (Ast.toString (infer env e2)) ^ " <> " ^ (Ast.toString (infer env e3))))))
  | Ast.And(e1, e2) ->
    let e1' = infer env e1 in
    let e2' = infer env e2 in
    check_equal env e1' e2';
    check_equal env e1' Ast.BoolType;
    Ast.BoolType
  | Ast.Or(e1, e2) ->
    let [e1'; e2'] = List.map (infer env) [e1; e2] in
    check_equal env e1' e2';
    check_equal env e1' Ast.BoolType;
    Ast.BoolType
  | Ast.Op(rator, rands) ->
    let (x, Ast.Prod s, t) = infer_pi env rator in
    let e = List.map (infer env) rands in
    apply_list (List.map (check_equal env) s) e;
    Ast.subst [(x, Ast.Prod rands)] t
  | Ast.Prod x -> Ast.Prod (List.map (infer env) x)
  | Ast.Let(x, typ, e) ->
    let temp = !env in
    extend x typ env;
    let t = infer env e in
    (try check_equal env t typ
     with Failure s ->
       env := temp;
       raise (Failure ("Ast.Let binding does not type-check\n" ^ s)));
    env := temp;
    t
  | Ast.IntType ->
    Ast.Star
  | Ast.BoolType ->
    Ast.Star
  | Ast.List(typ, len) ->
    (match infer env typ, infer env len with
    | Ast.Star, Ast.IntType -> Ast.Star
    | _ -> raise (Failure "Input does not type-check as list\n"))
  | Ast.Nil e ->
    let t = fst (normalize env e) in
    check_equal env (infer env t) Ast.Star;
    Ast.List(t, Ast.Int 0)
  | Ast.Cons(len, typ, el, rest) ->
    let len' = fst (normalize env len) in
    let typ' = fst (normalize env typ) in
    check_equal env (infer env typ') Ast.Star;
    check_equal env (infer env len') Ast.IntType;
    let el' = infer env el in
    check_equal env typ' el';
    (match len', infer env rest with
    | Ast.Int i, Ast.List(t, Ast.Int j) ->
      check_equal env t typ';
      (try assert (i = j+1)
       with Assert_failure s -> raise (Failure "Ast.List lengths do not type-check\n"));
      Ast.List(typ', len')
    | _, Ast.List(t, _) ->
      Ast.List(typ', len')
    | _ -> raise (Failure "Input does not type-check\n"))
  | Ast.IsNil e ->
    (match infer env e with
    | Ast.List(_, Ast.Int i) -> Ast.BoolType
    | _ -> raise (Failure "Input does not type-check\n"))
  | Ast.Head e ->
    (match infer env e with
    | Ast.List(t, _) ->
      t
    | _ -> raise (Failure "Ast.Head does not type-check\n"))
  | Ast.Tail e ->
    (match infer env e with
    | Ast.List(t, Ast.Int i) as t' ->
      if i = 0 then t'
      else Ast.List(t, Ast.Int (i-1))
    | Ast.List(t, a) ->
      Ast.List(t, Ast.Op(Ast.Var(Ast.String("-")), [a; Ast.Int 1]))
    | _ -> raise (Failure "Ast.Tail does not type-check\n"))
  | _ -> raise (Failure "General input does not type-check\n")

and infer_pi env e =
  let t = infer env e in
  (match fst (normalize env t) with
  | Ast.Pi a -> a
  | _ -> raise (Failure "Dependent function space expected\n"))

and check_equal env x y =
  if not (equal env x y) then raise (Failure ("Expressions are not equivalent\n" ^ (Ast.toString x) ^ " <> " ^ (Ast.toString y)))
