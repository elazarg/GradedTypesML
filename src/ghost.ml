(** Ghost state for tracking culprit sites - Section 4.3 *)

open Types

type site_id = int

module SiteSet = Set.Make(Int)

type ghost_state = {
  culprits: SiteSet.t;  (* Set of sites that contributed to imprecision *)
}

type augmented_type = graded_type * ghost_state

let empty_ghost = { culprits = SiteSet.empty }

let augment t = (t, empty_ghost)

let unaugment (t, _) = t

(** Add a site to ghost state *)
let add_site site (t, ghost) =
  (t, { culprits = SiteSet.add site ghost.culprits })

(** Union ghost states *)
let union_ghost g1 g2 =
  { culprits = SiteSet.union g1.culprits g2.culprits }

(** Join with ghost tracking *)
let join_with_ghost site (t1, g1) (t2, g2) =
  let t = Join.join t1 t2 in
  let culprits = SiteSet.union g1.culprits g2.culprits in
  (* Add current site if join created imprecision *)
  let culprits' = 
    match t with
    | Any _ when not (match t1, t2 with Any _, _ | _, Any _ -> true | _ -> false) ->
        SiteSet.add site culprits
    | _ -> culprits in
  (t, { culprits = culprits' })

(** Coerce with ghost tracking *)  
let coerce_with_ghost site b (t, g) =
  let t' = Subtype.coerce_to_base b t in
  let g' = 
    if grade_of t' > grade_of t then
      { culprits = SiteSet.add site g.culprits }
    else g in
  (t', g')

(** Pretty print ghost state *)
let pp_ghost g =
  let sites = SiteSet.elements g.culprits in
  "blamed sites: {" ^ 
  String.concat ", " (List.map string_of_int sites) ^ "}"

(** Augmented environment *)
module AugEnv = struct
  module M = Map.Make(String)
  
  type t = augmented_type M.t
  
  let empty = M.empty
  
  let lookup x env =
    try M.find x env
    with Not_found -> augment (Any Bot)
  
  let update x at env = M.add x at env
  
  let join site env1 env2 =
    let keys = 
      M.fold (fun k _ acc -> k :: acc) env1 [] @
      M.fold (fun k _ acc -> k :: acc) env2 [] in
    let keys = List.sort_uniq String.compare keys in
    List.fold_left (fun env k ->
      let at1 = lookup k env1 in
      let at2 = lookup k env2 in
      update k (join_with_ghost site at1 at2) env
    ) empty keys
  
  let equal env1 env2 =
    M.equal (fun (t1,_) (t2,_) -> type_eq t1 t2) env1 env2
    
  let pp env =
    M.fold (fun k (v,g) acc ->
      let type_str = match v with
        | Base (Int, Finite n) -> "Int^" ^ string_of_int n
        | Base (String, Finite n) -> "String^" ^ string_of_int n
        | Base (Bool, Finite n) -> "Bool^" ^ string_of_int n
        | Any (Finite n) -> "Any^" ^ string_of_int n
        | Any Bot -> "Any^⊥"
        | Any Inf -> "Any^∞"
        | _ -> "?" in
      let ghost_str = 
        if SiteSet.is_empty g.culprits then ""
        else " " ^ pp_ghost g in
      acc ^ k ^ ": " ^ type_str ^ ghost_str ^ ", "
    ) env "{ " ^ " }"
end
