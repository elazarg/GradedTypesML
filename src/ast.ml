(** AST for while-language - Section 3.1 *)

type value =
  | IntVal of int
  | StringVal of string
  | BoolVal of bool

type binop = Plus | And | Eq

type expr =
  | Lit of value
  | Var of string
  | Binop of binop * expr * expr
  | Not of expr

type stmt =
  | Skip
  | Assign of string * expr
  | Seq of stmt * stmt
  | If of expr * stmt * stmt
  | While of expr * stmt

(** Pretty printing *)
let rec pp_expr = function
  | Lit (IntVal n) -> string_of_int n
  | Lit (StringVal s) -> "\"" ^ s ^ "\""
  | Lit (BoolVal b) -> string_of_bool b
  | Var x -> x
  | Binop (Plus, e1, e2) -> "(" ^ pp_expr e1 ^ " + " ^ pp_expr e2 ^ ")"
  | Binop (And, e1, e2) -> "(" ^ pp_expr e1 ^ " && " ^ pp_expr e2 ^ ")"
  | Binop (Eq, e1, e2) -> "(" ^ pp_expr e1 ^ " = " ^ pp_expr e2 ^ ")"
  | Not e -> "!" ^ pp_expr e

let rec pp_stmt = function
  | Skip -> "skip"
  | Assign (x, e) -> x ^ " := " ^ pp_expr e
  | Seq (s1, s2) -> pp_stmt s1 ^ "; " ^ pp_stmt s2
  | If (e, s1, s2) -> 
      "if " ^ pp_expr e ^ " then " ^ pp_stmt s1 ^ " else " ^ pp_stmt s2
  | While (e, s) -> "while " ^ pp_expr e ^ " do " ^ pp_stmt s
  