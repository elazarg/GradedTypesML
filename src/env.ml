(** Abstract environments - maps from variables to types *)

open Types

module Env = Map.Make(String)

type t = graded_type Env.t

let empty = Env.empty

let lookup x env = 
  try Env.find x env
  with Not_found -> Any Bot  (* uninitialized variables *)

let update x t env = Env.add x t env

(** Pointwise join of environments *)
let join env1 env2 =
  let keys = 
    Env.fold (fun k _ acc -> k :: acc) env1 [] @
    Env.fold (fun k _ acc -> k :: acc) env2 [] in
  let keys = List.sort_uniq String.compare keys in
  List.fold_left (fun env k ->
    let t1 = lookup k env1 in
    let t2 = lookup k env2 in
    update k (Join.join t1 t2) env
  ) empty keys

(** Environment equality *)
let equal env1 env2 =
  Env.equal type_eq env1 env2

(** Pretty printing *)
let pp env =
  Env.fold (fun k v acc ->
    acc ^ k ^ ": " ^ 
    (match v with
     | Base (Int, Finite n) -> "Int^" ^ string_of_int n
     | Base (String, Finite n) -> "String^" ^ string_of_int n
     | Base (Bool, Finite n) -> "Bool^" ^ string_of_int n
     | Any (Finite n) -> "Any^" ^ string_of_int n
     | Any Bot -> "Any^⊥"
     | Any Inf -> "Any^∞"
     | _ -> "?") ^ ", "
  ) env "{ " ^ " }"
  