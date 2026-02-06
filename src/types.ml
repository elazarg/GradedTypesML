(** Core type definitions for graded types *)

type base = Int | String | Bool

type grade = 
  | Bot
  | Finite of int  
  | Inf

type graded_type =
  | Base of base * grade
  | Any of grade

(** Extract grade from a type *)
let grade_of = function
  | Base (_, g) -> g
  | Any g -> g

(** Grade comparison *)
let grade_leq g1 g2 = match (g1, g2) with
  | (Bot, _) -> true
  | (_, Inf) -> true
  | (Finite i, Finite j) -> i <= j
  | _ -> false

(** Grade equality *)
let grade_eq g1 g2 = match (g1, g2) with
  | (Bot, Bot) -> true
  | (Finite i, Finite j) -> i = j
  | (Inf, Inf) -> true
  | _ -> false

(** Maximum of two grades *)
let max_grade g1 g2 = match (g1, g2) with
  | (Bot, g) | (g, Bot) -> g
  | (Inf, _) | (_, Inf) -> Inf
  | (Finite i, Finite j) -> Finite (max i j)

(** Increment grade by 1 *)
let succ_grade = function
  | Bot -> Finite 0
  | Finite n -> Finite (n + 1)
  | Inf -> Inf

(** Type equality *)
let type_eq t1 t2 = match (t1, t2) with
  | (Base (b1, g1), Base (b2, g2)) -> b1 = b2 && grade_eq g1 g2
  | (Any g1, Any g2) -> grade_eq g1 g2
  | _ -> false

(** Pretty printing *)
let pp_base = function
  | Int -> "Int"
  | String -> "String"
  | Bool -> "Bool"

let pp_grade = function
  | Bot -> "⊥"
  | Finite n -> string_of_int n
  | Inf -> "∞"

let pp_type = function
  | Base (b, g) -> pp_base b ^ "^" ^ pp_grade g
  | Any g -> "Any^" ^ pp_grade g
