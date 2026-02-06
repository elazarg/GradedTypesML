(** Abstract interpretation with ghost state tracking *)

open Types
open Ast
open Ghost

let site_counter = ref 0
let fresh_site () = incr site_counter; !site_counter

(** Coercion cost: 0 if the type already has the target base, 1 otherwise *)
let coercion_cost b = function
  | Base (b', _) when b = b' -> 0
  | _ -> 1

(** Select the best augmented interpretation(s) from (aug_type, cost) pairs.
    Prefer the fewest costly coercions; join ties with ghost tracking. *)
let best_interp_ghost site candidates =
  let min_cost = List.fold_left (fun acc (_, c) -> min acc c) max_int candidates in
  let best = List.filter (fun (_, c) -> c = min_cost) candidates in
  match best with
  | [] -> failwith "best_interp_ghost: empty candidates"
  | [(at, _)] -> at
  | _ ->
      List.fold_left
        (fun acc (at, _) -> join_with_ghost site acc at)
        (fst (List.hd best)) (List.tl best)

(** Type expressions with ghost state *)
let rec type_expr_ghost env = function
  | Lit (IntVal _) -> augment (Base (Int, Finite 0))
  | Lit (StringVal _) -> augment (Base (String, Finite 0))
  | Lit (BoolVal _) -> augment (Base (Bool, Finite 0))

  | Var x -> AugEnv.lookup x env

  | Binop (Plus, e1, e2) ->
      let site = fresh_site () in
      let at1 = type_expr_ghost env e1 in
      let at2 = type_expr_ghost env e2 in
      let try_interp b =
        let at1' = coerce_with_ghost site b at1 in
        let at2' = coerce_with_ghost site b at2 in
        let (t1', g1) = at1' in
        let (t2', g2) = at2' in
        let result = (Base (b, max_grade (grade_of t1') (grade_of t2')),
                      union_ghost g1 g2) in
        let cost = coercion_cost b (fst at1) + coercion_cost b (fst at2) in
        (result, cost) in
      best_interp_ghost site [try_interp Int; try_interp String]

  | Binop (And, e1, e2) ->
      let site = fresh_site () in
      let at1 = type_expr_ghost env e1 in
      let at2 = type_expr_ghost env e2 in
      let at1' = coerce_with_ghost site Bool at1 in
      let at2' = coerce_with_ghost site Bool at2 in
      let (t1', g1) = at1' in
      let (t2', g2) = at2' in
      (Base (Bool, max_grade (grade_of t1') (grade_of t2')),
       union_ghost g1 g2)

  | Binop (Eq, e1, e2) ->
      let site = fresh_site () in
      let at1 = type_expr_ghost env e1 in
      let at2 = type_expr_ghost env e2 in
      let try_base b =
        let at1' = coerce_with_ghost site b at1 in
        let at2' = coerce_with_ghost site b at2 in
        let (t1', g1) = at1' in
        let (t2', g2) = at2' in
        let result = (Base (Bool, max_grade (grade_of t1') (grade_of t2')),
                      union_ghost g1 g2) in
        let cost = coercion_cost b (fst at1) + coercion_cost b (fst at2) in
        (result, cost) in
      best_interp_ghost site [try_base Int; try_base String; try_base Bool]
  
  | Not e ->
      let site = fresh_site () in
      let at = type_expr_ghost env e in
      let (t', g) = coerce_with_ghost site Bool at in
      (Base (Bool, grade_of t'), g)

(** Transform statements with ghost *)
let rec transform_ghost env = function
  | Skip -> env
  
  | Assign (x, e) ->
      let at = type_expr_ghost env e in
      AugEnv.update x at env
  
  | Seq (s1, s2) ->
      let env1 = transform_ghost env s1 in
      transform_ghost env1 s2
  
  | If (e, s1, s2) ->
      let site = fresh_site () in
      let _ = coerce_with_ghost site Bool (type_expr_ghost env e) in
      let env1 = transform_ghost env s1 in
      let env2 = transform_ghost env s2 in
      AugEnv.join site env1 env2
  
  | While (e, s) ->
      let site = fresh_site () in
      let f env' =
        let _ = coerce_with_ghost site Bool (type_expr_ghost env' e) in
        let env_body = transform_ghost env' s in
        AugEnv.join site env env_body in
      fixpoint_ghost f env

and fixpoint_ghost f env =
  let rec iterate env n =
    if n > 100 then env  (* iteration limit *)
    else
      let env' = f env in
      if AugEnv.equal env env' then env
      else iterate env' (n + 1)
  in iterate env 0
