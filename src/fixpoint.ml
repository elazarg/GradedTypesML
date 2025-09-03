(** Fixed point computation with widening *)

open Types
open Env

(** Simple iteration limit to ensure termination *)
let iteration_limit = 100

(** Fixed point with iteration counting *)
let rec fixpoint_bounded f env n =
  if n >= iteration_limit then
    (* Widening: jump to Any^∞ for non-stable variables *)
    map (fun t -> 
      if grade_of t = Inf then t else Any Inf
    ) env
  else
    let env' = f env in
    if equal env env' then env
    else fixpoint_bounded f env' (n + 1)

let fixpoint f env = fixpoint_bounded f env 0

(** Alternative: widening at grade k *)
let widen k t =
  match grade_of t with
  | Finite n when n > k -> 
      (match t with
       | Base (b, _) -> Base (b, Finite k)
       | Any _ -> Any (Finite k))
  | _ -> t

let fixpoint_with_widening k f env =
  let rec iterate env n =
    let env' = f env in
    let env'' = 
      if n > k then map (widen k) env'
      else env' in
    if equal env env'' then env
    else iterate env'' (n + 1)
  in iterate env 0
