(** Subtyping relation - Section 2 of paper *)

open Types

(** Check if t1 is a subtype of t2.
    This is the transitive closure of the base rules. *)
let subtype t1 t2 =
  if type_eq t1 t2 then true
  else match (t1, t2) with
  (* Any^g1 <= Any^g2 iff g1 <= g2. This covers Bot and Inf via grade_leq *)
  | (Any g1, Any g2) -> grade_leq g1 g2

  (* Base(b, g1) <= Any(g2) iff g1 <= g2. This covers Inf via grade_leq *)
  | (Base (_, g1), Any g2) -> grade_leq g1 g2

  (* Any(g1) <= Base(b, g2) iff succ(g1) <= g2. This covers Bot via grade_leq *)
  | (Any g1, Base (_, g2)) -> grade_leq (succ_grade g1) g2

  (* Base(b1, g1) <= Base(b2, g2) iff b1=b2 and g1<=g2 *)
  | (Base (b1, g1), Base (b2, g2)) -> b1 = b2 && grade_leq g1 g2

(** Coerce to base type - the ceil operator from Section 3.2 *)
let coerce_to_base b t =
  match t with
  | Base (b', _) when b = b' -> t
  | Base (_, g) -> Base (b, succ_grade g)
  | Any g -> Base (b, succ_grade g)
