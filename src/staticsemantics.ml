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
  | Var x ->
    (match
	(try lookup_value x !env
	 with Not_found -> raise (Failure "unknown identifier\n"))
     with
     | None -> (Var x, env)
     | Some e -> (fst (normalize env e), env))
  | Star ->
    (Star, env)
  | Pi a ->
    (Pi (normalize_abs env a), env)
  | Lambda a ->
    (Lambda (normalize_abs env a), env)
  | App(e1, e2) ->
    let e2' = fst (normalize env e2) in
    (match fst (normalize env e1) with
    | Lambda (x, _, e1') ->
      (fst (normalize env (subst [(x, e2')] e1')), env)
    | e1 ->
      (App(e1, e2)), env)
  | Int i ->
    (Int i, env)
  | Bool b ->
    (Bool b, env)
  | Ann(e1, e2) ->
    (Ann (fst (normalize env e1), fst (normalize env e2)), env)
  | If(e1, e2, e3) ->
    (match fst (normalize env e1) with
    | Bool 1 -> normalize env e2
    | Bool 0 -> normalize env e3)
  | And(e1, e2) ->
    (match fst (normalize env e1) with
    | Bool 0 as t -> (t, env)
    | Bool 1 -> normalize env e2)
  | Or(e1, e2) ->
    (match fst (normalize env e1) with
    | Bool 1 as t -> (t, env)
    | Bool 0 -> normalize env e2)
  | Op(rator, rands) ->
    let rator' = fst (normalize env rator) in
    (match rator', rands with
    | BinaryOp f, [a;b] ->
      (f (fst (normalize env a), fst (normalize env b)), env)
    | UnaryOp f, [a] ->
      (f (fst (normalize env a))), env)
  | Prod x ->
    (Prod (List.map fst (List.map (normalize env) x)), env)
  | Let (x, t, e) ->
    let t' = fst (normalize env t) in
    let e' = fst (normalize env e) in
    extend x t' ~value:e' env;
    (Let (x, t', e'), env)
  | IntType ->
    (IntType, env)
  | BoolType ->
    (BoolType, env)
  | BinaryOp f as x -> (x, env)
  | UnaryOp f as x -> (x, env)
  | List(typ, len) ->
    (List (fst (normalize env typ), fst (normalize env len)), env)
  | Nil e -> (Nil (fst (normalize env e)), env)
  | Cons(len, typ, el, rest) ->
    (Cons (fst (normalize env len), fst (normalize env typ), fst (normalize env el), fst (normalize env rest)), env)
  | IsNil e ->
    (match fst (normalize env e) with
    | Nil a as t -> (Bool 1, env)
    | Cons(_,_,_,_) -> (Bool 0, env)
    | _ -> raise (Failure "Input cannot normalize\n"))
  | Head e ->
    (match fst (normalize env e) with
    | Cons(_,_,e,_) -> (e, env)
    | _ -> raise (Failure "Cannot normalize head of anything other than non-empty list\n"))
  | Tail e ->
    (match fst (normalize env e) with
    | Cons(_,_,_,e) -> (e, env)
    | Nil e -> (Nil e, env)
    | _ -> raise (Failure "Cannot normalize tail of anything other than a list\n"))
  | _ -> raise (Failure "Input cannot normalize\n")

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
    | Var x1, Var x2 -> x1 = x2
    | App(d1, d2), App(f1, f2) -> equal' d1 f1 && equal' d2 f2
    | Star, Star -> true
    | Pi a1, Pi a2 -> equal_abs a1 a2
    | Lambda a1, Lambda a2 -> equal_abs a1 a2
    | Int i, Int j -> i = j
    | Bool b, Bool b' -> b = b'
    | Ann(d1, d2), Ann(f1, f2) ->
      equal' d1 f1 && equal' d2 f2
    | Op(r, rands), Op(r', rands') ->
      equal' r r' && all_true (apply_list (List.map equal' rands) rands')
    | Let a1, Let a2 ->
      equal_abs a1 a2
    | IntType, IntType -> true
    | BoolType, BoolType -> true
    | Prod a, Prod b ->
      (match a, b with
      | [], [] -> true
      | [x], [y] -> equal' x y
      | x::xs, y::ys -> equal' (Prod xs) (Prod ys))
    | List(a, b), List(x, y) ->
      equal' a x && equal' b y
    | Nil a, Nil b ->
      equal' a b
    | Cons(a1, b1, c1, d1), Cons(a2, b2, c2, d2) ->
      equal' a1 a2 && equal' b1 b2 && equal' c1 c2 && equal' d1 d1
    | IsNil a, IsNil b ->
      equal' a b
    | Head a, Head b ->
      equal' a b
    | Tail a, Tail b ->
      equal' a b
    | _, _ -> false)

  and equal_abs (x, t, e1) (x', t', e2) =
    let z = Var (fresh x) in
    equal' t t' && (equal' (subst [(x, z)] e1) (subst [(x', z)] e2))
  in
  equal' (fst (normalize env e1)) (fst (normalize env e2))

let rec infer env = function
  | Var x ->
    (try lookup_typ x !env
     with Not_found -> raise (Failure "unknown identifier \n"))
  | Star -> Star
  | Pi (x, t, e) ->
    let t' = infer env t in
    let temp = !env in
    extend x t env;
    let e' = infer env e in
    env := temp;
    (match t', e' with
    | Star, Star -> Star
    | _, _ -> raise (Failure "Invalid type in dependent function space \n"))
  | Lambda (x, t, e) ->
    let t' = infer env t in
    let temp = !env in
    extend x t env;
    let e' =
      (try infer env e
       with Failure s ->
	 env := temp;
	 raise (Failure ("Input does not type-check\n" ^ s)))
    in
    env := temp;
    Pi (x, t, e')
  | App(e1, e2) ->
    let (x, s, t) = infer_pi env e1 in
    let e2' = infer env e2 in
    check_equal env s e2';
    subst [(x, e2)] t
  | Int i -> IntType
  | Bool b -> BoolType
  | Ann(e1, e2) ->
    let t = infer env e1 in
    check_equal env t e2;
    t
  | If(e1, e2, e3) ->
    (try check_equal env (infer env e1) BoolType;
	 check_equal env (infer env e2) (infer env e3);
	 infer env e2
     with Failure s ->
       check_equal env (infer env e1) BoolType;
       (match (infer env e2), (infer env e3) with
       | List(t, a), List(t', b) ->
	 check_equal env t t';
	 List(t', b)
       | List(_, Int i), List(_, Int j) -> raise (Failure "If statement on lists does not type-check\n")
       | _, _ -> raise (Failure ("If statement does not type-check\n" ^ (toString (infer env e2)) ^ " <> " ^ (toString (infer env e3))))))
  | And(e1, e2) ->
    let e1' = infer env e1 in
    let e2' = infer env e2 in
    check_equal env e1' e2';
    check_equal env e1' BoolType;
    BoolType
  | Or(e1, e2) ->
    let [e1'; e2'] = List.map (infer env) [e1; e2] in
    check_equal env e1' e2';
    check_equal env e1' BoolType;
    BoolType
  | Op(rator, rands) ->
    let (x, Prod s, t) = infer_pi env rator in
    let e = List.map (infer env) rands in
    apply_list (List.map (check_equal env) s) e;
    subst [(x, Prod rands)] t
  | Prod x -> Prod (List.map (infer env) x)
  | Let(x, typ, e) ->
    let temp = !env in
    extend x typ env;
    let t = infer env e in
    (try check_equal env t typ
     with Failure s ->
       env := temp;
       raise (Failure ("Let binding does not type-check\n" ^ s)));
    env := temp;
    t
  | IntType ->
    Star
  | BoolType ->
    Star
  | List(typ, len) ->
    (match infer env typ, infer env len with
    | Star, IntType -> Star
    | _ -> raise (Failure "Input does not type-check as list\n"))
  | Nil e ->
    let t = fst (normalize env e) in
    check_equal env (infer env t) Star;
    List(t, Int 0)
  | Cons(len, typ, el, rest) ->
    let len' = fst (normalize env len) in
    let typ' = fst (normalize env typ) in
    check_equal env (infer env typ') Star;
    check_equal env (infer env len') IntType;
    let el' = infer env el in
    check_equal env typ' el';
    (match len', infer env rest with
    | Int i, List(t, Int j) ->
      check_equal env t typ';
      (try assert (i = j+1)
       with Assert_failure s -> raise (Failure "List lengths do not type-check\n"));
      List(typ', len')
    | _, List(t, _) ->
      List(typ', len')
    | _ -> raise (Failure "Input does not type-check\n"))
  | IsNil e ->
    (match infer env e with
    | List(_, Int i) -> BoolType
    | _ -> raise (Failure "Input does not type-check\n"))
  | Head e ->
    (match infer env e with
    | List(t, _) ->
      t
    | _ -> raise (Failure "Head does not type-check\n"))
  | Tail e ->
    (match infer env e with
    | List(t, Int i) as t' ->
      if i = 0 then t'
      else List(t, Int (i-1))
    | List(t, a) ->
      List(t, Op(Var(String("-")), [a; Int 1]))
    | _ -> raise (Failure "Tail does not type-check\n"))
  | _ -> raise (Failure "General input does not type-check\n")

and infer_pi env e =
  let t = infer env e in
  (match fst (normalize env t) with
  | Pi a -> a
  | _ -> raise (Failure "Dependent function space expected\n"))

and check_equal env x y =
  if not (equal env x y) then raise (Failure ("Expressions are not equivalent\n" ^ (Ast.toString x) ^ " <> " ^ (Ast.toString y)))
