(** Property-based tests for the graded type system *)
open Graded_types
open Types
open QCheck

let pp_grade = function
  | Bot -> "⊥"
  | Finite n -> string_of_int n
  | Inf -> "∞"

let pp_type = function
  | Base (Int, g) -> "Int^" ^ pp_grade g
  | Base (String, g) -> "String^" ^ pp_grade g
  | Base (Bool, g) -> "Bool^" ^ pp_grade g
  | Any g -> "Any^" ^ pp_grade g

let pp_pair (t1, t2) =
  Printf.sprintf "(%s, %s)" (pp_type t1) (pp_type t2)

let pp_triple (t1, t2, t3) =
  Printf.sprintf "(%s, %s, %s)" (pp_type t1) (pp_type t2) (pp_type t3)

let pp_grade_pair (g1, g2) =
  Printf.sprintf "(%s, %s)" (pp_grade g1) (pp_grade g2)

(** Generators *)
let gen_base = Gen.oneof [
  Gen.return Int;
  Gen.return String;
  Gen.return Bool;
]

let gen_finite_grade = Gen.map (fun n -> Finite n) (Gen.int_range 0 10)

let gen_grade = Gen.oneof [
  Gen.return Bot;
  gen_finite_grade;
  Gen.return Inf;
]

let gen_type =
  Gen.oneof [
    Gen.map2 (fun b g -> Base (b, g)) gen_base gen_grade;
    Gen.map (fun g -> Any g) gen_grade;
  ]

(** Arbitraries for QCheck *)
let arb_type = QCheck.make ~print:pp_type gen_type
let arb_pair_type = QCheck.make ~print:pp_pair (Gen.pair gen_type gen_type)
let arb_triple_type = QCheck.make ~print:pp_triple (Gen.triple gen_type gen_type gen_type)
let arb_pair_finite_grade = QCheck.make ~print:pp_grade_pair (Gen.pair gen_finite_grade gen_finite_grade)


(** Properties *)

(* Subtyping is reflexive *)
let prop_subtype_refl =
  Test.make ~name:"subtype reflexive" arb_type
    (fun t -> Subtype.subtype t t)

(* Update the test *)
let prop_subtype_antisym =
  Test.make ~name:"subtype antisymmetric"
    arb_pair_type
    (fun (t1, t2) ->
      if Subtype.subtype t1 t2 && Subtype.subtype t2 t1
      then type_eq t1 t2
      else true)

(* Join is commutative *)
let prop_join_comm =
  Test.make ~name:"join commutative" arb_pair_type
    (fun (t1, t2) ->
      type_eq (Join.join t1 t2) (Join.join t2 t1))

(* Join is associative *)
let prop_join_assoc =
  Test.make ~name:"join associative" arb_triple_type
    (fun (t1, t2, t3) ->
      type_eq
        (Join.join (Join.join t1 t2) t3)
        (Join.join t1 (Join.join t2 t3)))

(* Join is idempotent *)
let prop_join_idemp =
  Test.make ~name:"join idempotent" arb_type
    (fun t -> type_eq (Join.join t t) t)

(* Join is least upper bound *)
let prop_join_lub =
  Test.make ~name:"join is LUB" arb_pair_type
    (fun (t1, t2) ->
      let j = Join.join t1 t2 in
      Subtype.subtype t1 j && Subtype.subtype t2 j)

(* Grade monotonicity *)
let prop_grade_monotone =
  Test.make ~name:"grade monotone" arb_pair_finite_grade
    (fun (g1, g2) ->
      if grade_leq g1 g2 then
        Subtype.subtype (Any g1) (Any g2)
      else true)

(* Run all tests *)
let run_tests () =
  print_endline "--- Running Property-Based Tests ---";
  let tests = [
    prop_subtype_refl;
    prop_subtype_antisym;
    prop_join_comm;
    prop_join_assoc;
    prop_join_idemp;
    prop_join_lub;
    prop_grade_monotone;
  ] in
  let property_results = QCheck_runner.run_tests ~verbose:true tests in

  print_endline "\n--- Running Paper Examples ---";
  Paper_examples.run_all ();

  if property_results = 0 then
    exit 0
  else
    exit 1

let () = run_tests ()

