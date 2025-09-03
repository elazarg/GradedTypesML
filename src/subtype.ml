(** Subtyping relation - Section 2 of paper *)

open Types

(** Check if t1 is a subtype of t2 *)
let subtype t1 t2 =
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
  match t with
  | Base (b', _) when b = b' -> t
  | Base (_, g) -> Base (b, succ_grade g)  (* different base *)
  | Any g -> Base (b, succ_grade g)  (* downcast from Any *)
