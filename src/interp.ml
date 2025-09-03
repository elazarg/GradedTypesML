(** Abstract interpretation - Section 3.2 *)

open Types
open Ast
open Subtype

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
      (* Try both interpretations *)
      let try_int =
        let t1' = coerce_to_base Int t1 in
        let t2' = coerce_to_base Int t2 in
        Base (Int, max_grade (grade_of t1') (grade_of t2')) in
      let try_string =
        let t1' = coerce_to_base String t1 in
        let t2' = coerce_to_base String t2 in
        Base (String, max_grade (grade_of t1') (grade_of t2')) in
      Join.join try_int try_string
  
  | Binop (And, e1, e2) ->
      let t1 = type_expr env e1 in
      let t2 = type_expr env e2 in
      let t1' = coerce_to_base Bool t1 in
      let t2' = coerce_to_base Bool t2 in
      Base (Bool, max_grade (grade_of t1') (grade_of t2'))
  
  | Binop (Eq, e1, e2) ->
      let t1 = type_expr env e1 in
      let t2 = type_expr env e2 in
      (* Try all three base types *)
      let try_base b =
        let t1' = coerce_to_base b t1 in
        let t2' = coerce_to_base b t2 in
        Base (Bool, max_grade (grade_of t1') (grade_of t2')) in
      Join.join (try_base Int) 
        (Join.join (try_base String) (try_base Bool))
  
  | Not e ->
      let t = type_expr env e in
      let t' = coerce_to_base Bool t in
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
      let _ = coerce_to_base Bool (type_expr env e) in
      let env1 = transform env s1 in
      let env2 = transform env s2 in
      Env.join env1 env2
  
  | While (e, s) ->
      (* Fixed point computation *)
      let f env' =
        let _ = coerce_to_base Bool (type_expr env' e) in
        let env_body = transform env' s in
        Env.join env env_body in
      fixpoint f env

and fixpoint f env =
  let env' = f env in
  if Env.equal env env' then env
  else fixpoint f env'
  