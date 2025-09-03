(** Property-based tests for the graded type system *)
open Graded_types

open QCheck

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

(** Properties *)

(* Subtyping is reflexive *)
let prop_subtype_refl =
  Test.make ~name:"subtype reflexive" gen_type
    (fun t -> Subtype.subtype t t)

(* Subtyping is antisymmetric *)
let prop_subtype_antisym =
  Test.make ~name:"subtype antisymmetric" 
    (Gen.pair gen_type gen_type)
    (fun (t1, t2) ->
      if Subtype.subtype t1 t2 && Subtype.subtype t2 t1
      then type_eq t1 t2
      else true)

(* Join is commutative *)
let prop_join_comm =
  Test.make ~name:"join commutative"
    (Gen.pair gen_type gen_type)
    (fun (t1, t2) ->
      type_eq (Join.join t1 t2) (Join.join t2 t1))

(* Join is associative *)
let prop_join_assoc =
  Test.make ~name:"join associative"
    (Gen.triple gen_type gen_type gen_type)
    (fun (t1, t2, t3) ->
      type_eq 
        (Join.join (Join.join t1 t2) t3)
        (Join.join t1 (Join.join t2 t3)))

(* Join is idempotent *)
let prop_join_idemp =
  Test.make ~name:"join idempotent" gen_type
    (fun t -> type_eq (Join.join t t) t)

(* Join is least upper bound *)
let prop_join_lub =
  Test.make ~name:"join is LUB"
    (Gen.pair gen_type gen_type)
    (fun (t1, t2) ->
      let j = Join.join t1 t2 in
      Subtype.subtype t1 j && Subtype.subtype t2 j)

(* Grade monotonicity *)
let prop_grade_monotone =
  Test.make ~name:"grade monotone"
    (Gen.pair gen_finite_grade gen_finite_grade)
    (fun (g1, g2) ->
      if grade_leq g1 g2 then
        Subtype.subtype (Any g1) (Any g2)
      else true)

(* Run all tests *)
let run_tests () =
  let tests = [
    prop_subtype_refl;
    prop_subtype_antisym;
    prop_join_comm;
    prop_join_assoc;
    prop_join_idemp;
    prop_join_lub;
    prop_grade_monotone;
  ] in
  List.iter (fun test ->
    QCheck_runner.run_tests ~verbose:true [test] |> ignore
  ) tests
