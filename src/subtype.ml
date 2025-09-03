(** Subtyping relation - Section 2 of paper *)

open Types

(** Check if t1 is a subtype of t2 *)
let rec subtype t1 t2 = 
  match (t1, t2) with
  (* Rule (grade): X^i ≤ Any^i *)
  | (Base (_, i), Any j) when grade_eq i j -> true
  
  (* Rule (Step): Any^i ≤ X^(i+1) *)
  | (Any i, Base (_, j)) when grade_eq (succ_grade i) j -> true
  
  (* Reflexivity *)
  | _ when type_eq t1 t2 -> true
  
  (* Monotone grades for base types *)
  | (Base (b1, i), Base (b2, j)) when b1 = b2 -> grade_leq i j
  
  (* Monotone grades for Any *)
  | (Any i, Any j) -> grade_leq i j
  
  (* No other subtyping *)
  | _ -> false

(** Coerce to base type - the ceil operator from Section 3.2 *)
let coerce_to_base b t =
  (* Find least X-supertype of t *)
  let rec find_grade g =
    let candidate = Base (b, g) in
    if subtype t candidate then
      candidate
    else
      match g with
      | Finite n when n < 100 -> find_grade (Finite (n + 1))  (* arbitrary bound *)
      | _ -> Base (b, Inf)  (* fallback *)
  in
  match t with
  | Base (b', g) when b = b' -> t  (* already correct base *)
  | Base (_, g) -> Base (b, succ_grade g)  (* different base *)
  | Any g -> Base (b, succ_grade g)  (* downcast from Any *)
  