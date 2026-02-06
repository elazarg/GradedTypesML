(** Abstract interpretation functor, parameterized over coercion strategy *)

open Types
open Ast

module type COERCION = sig
  val coerce_to_base : base -> graded_type -> graded_type
end

module Make (C : COERCION) = struct

  (** Coercion cost: 0 if the type already has the target base, 1 otherwise *)
  let coercion_cost b = function
    | Base (b', _) when b = b' -> 0
    | _ -> 1

  (** Select the best interpretation(s) from a list of (result_type, cost) pairs.
      Prefer the interpretation with the fewest costly coercions.
      If tied, join the tied results. *)
  let best_interp candidates =
    let min_cost = List.fold_left (fun acc (_, c) -> min acc c) max_int candidates in
    let best = List.filter (fun (_, c) -> c = min_cost) candidates in
    match best with
    | [] -> failwith "best_interp: empty candidates"
    | [(t, _)] -> t
    | _ -> List.fold_left (fun acc (t, _) -> Join.join acc t) (fst (List.hd best)) (List.tl best)

  (** Type of expressions *)
  let rec type_expr env = function
    (* Literals at grade 0 *)
    | Lit (IntVal _) -> Base (Int, Finite 0)
    | Lit (StringVal _) -> Base (String, Finite 0)
    | Lit (BoolVal _) -> Base (Bool, Finite 0)

    (* Variable lookup *)
    | Var x -> Env.lookup x env

    (* Binary operations *)
    | Binop (Plus, e1, e2) ->
        let t1 = type_expr env e1 in
        let t2 = type_expr env e2 in
        let try_interp b =
          let t1' = C.coerce_to_base b t1 in
          let t2' = C.coerce_to_base b t2 in
          let result = Base (b, max_grade (grade_of t1') (grade_of t2')) in
          let cost = coercion_cost b t1 + coercion_cost b t2 in
          (result, cost) in
        best_interp [try_interp Int; try_interp String]

    | Binop (And, e1, e2) ->
        let t1 = type_expr env e1 in
        let t2 = type_expr env e2 in
        let t1' = C.coerce_to_base Bool t1 in
        let t2' = C.coerce_to_base Bool t2 in
        Base (Bool, max_grade (grade_of t1') (grade_of t2'))

    | Binop (Eq, e1, e2) ->
        let t1 = type_expr env e1 in
        let t2 = type_expr env e2 in
        let try_base b =
          let t1' = C.coerce_to_base b t1 in
          let t2' = C.coerce_to_base b t2 in
          let result = Base (Bool, max_grade (grade_of t1') (grade_of t2')) in
          let cost = coercion_cost b t1 + coercion_cost b t2 in
          (result, cost) in
        best_interp [try_base Int; try_base String; try_base Bool]

    | Not e ->
        let t = type_expr env e in
        let t' = C.coerce_to_base Bool t in
        Base (Bool, grade_of t')

  (** Transform statements *)
  let rec transform env = function
    | Skip -> env

    | Assign (x, e) ->
        let t = type_expr env e in
        Env.update x t env

    | Seq (s1, s2) ->
        let env1 = transform env s1 in
        transform env1 s2

    | If (e, s1, s2) ->
        let _ = C.coerce_to_base Bool (type_expr env e) in
        let env1 = transform env s1 in
        let env2 = transform env s2 in
        Env.join env1 env2

    | While (e, s) ->
        (* Fixed point computation with widening *)
        let f env' =
          let _ = C.coerce_to_base Bool (type_expr env' e) in
          let env_body = transform env' s in
          Env.join env env_body in
        Fixpoint.fixpoint f env

end
