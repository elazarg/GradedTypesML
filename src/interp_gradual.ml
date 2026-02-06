(** Abstract interpretation — gradual typing as collapsed graded typing.
    The only difference: any actual coercion jumps to grade ∞ (= dyn). *)

open Types

let succ_grade_gradual = function
  | Bot -> Finite 0    (* uninitialized → ground, same as graded *)
  | _ -> Inf           (* any actual coercion → dynamic *)

let coerce_to_base_gradual b t = match t with
  | Base (b', _) when b = b' -> t
  | Base (_, g) -> Base (b, succ_grade_gradual g)
  | Any g -> Base (b, succ_grade_gradual g)

include Interp_common.Make(struct
  let coerce_to_base = coerce_to_base_gradual
end)
