(** Join operation - Section 2, Figure 3 *)

open Types

(** Promote type to grade r *)
let promote r t =
  let g = grade_of t in
  if grade_eq g r then t
  else if grade_leq g r then Any r
  else failwith "Cannot promote to lower grade"

(** Same-grade join *)
let same_grade_join t1 t2 =
  match (t1, t2) with
  (* Same base type - idempotent *)
  | (Base (b1, g), Base (b2, _)) when b1 = b2 -> t1
  
  (* Different base types - heterogeneous merge *)
  | (Base (_, g), Base (_, _)) -> Any g
  
  (* Any absorbs everything *)
  | (Any g, _) | (_, Any g) -> Any g

(** Full join operation *)
let join t1 t2 =
  match (t1, t2) with
  (* Top absorption *)
  | (Any Inf, _) | (_, Any Inf) -> Any Inf
  
  (* Normal case: promote then join *)
  | _ ->
      let r = max_grade (grade_of t1) (grade_of t2) in
      let t1' = promote r t1 in
      let t2' = promote r t2 in
      same_grade_join t1' t2'

(** Join is commutative *)
let join_comm t1 t2 = 
  type_eq (join t1 t2) (join t2 t1)

(** Join is associative *)
let join_assoc t1 t2 t3 =
  type_eq (join (join t1 t2) t3) (join t1 (join t2 t3))

(** Join is idempotent *)
let join_idemp t =
  type_eq (join t t) t
  