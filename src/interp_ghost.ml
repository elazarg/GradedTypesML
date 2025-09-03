(** Abstract interpretation with ghost state tracking *)

open Types
open Ast
open Ghost

let site_counter = ref 0
let fresh_site () = incr site_counter; !site_counter

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
      (* Try both interpretations *)
      let try_int =
        let at1' = coerce_with_ghost site Int at1 in
        let at2' = coerce_with_ghost site Int at2 in
        let (t1', g1) = at1' in
        let (t2', g2) = at2' in
        (Base (Int, max_grade (grade_of t1') (grade_of t2')),
         union_ghost g1 g2) in
      let try_string =
        let at1' = coerce_with_ghost site String at1 in
        let at2' = coerce_with_ghost site String at2 in
        let (t1', g1) = at1' in
        let (t2', g2) = at2' in
        (Base (String, max_grade (grade_of t1') (grade_of t2')),
         union_ghost g1 g2) in
      join_with_ghost site try_int try_string
  
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
        (Base (Bool, max_grade (grade_of t1') (grade_of t2')),
         union_ghost g1 g2) in
      let r1 = try_base Int in
      let r2 = try_base String in
      let r3 = try_base Bool in
      join_with_ghost site (join_with_ghost site r1 r2) r3
  
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
