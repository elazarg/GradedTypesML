(** Abstract interpretation — graded typing (Section 3.2) *)

include Interp_common.Make(struct
  let coerce_to_base = Subtype.coerce_to_base
end)
