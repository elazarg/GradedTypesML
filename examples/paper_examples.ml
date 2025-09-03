(** Examples from Section 5 of the paper *)

open Ast
open Interp

(** Example 1: literal assignment *)
let ex1 = Assign ("x", Lit (IntVal 5))

let test_ex1 () =
  let env = Env.empty in
  let env' = transform env ex1 in
  assert (Env.lookup "x" env' = Base (Int, Finite 0));
  print_endline "Example 1: ✓"

(** Example 2: simple heterogeneous branch *)
let ex2 = 
  If (Var "b",
      Assign ("x", Lit (IntVal 5)),
      Assign ("x", Lit (StringVal "hi")))

let test_ex2 () =
  let env = Env.update "b" (Base (Bool, Finite 0)) Env.empty in
  let env' = transform env ex2 in
  assert (Env.lookup "x" env' = Any (Finite 0));
  print_endline "Example 2: ✓"

(** Example 3: correlated branches *)
let ex3 =
  Seq (
    If (Var "b",
        Assign ("x", Lit (IntVal 5)),
        Assign ("x", Lit (StringVal "hello"))),
    If (Var "b",
        Assign ("x", Binop (Plus, Var "x", Lit (IntVal 2))),
        Assign ("x", Binop (Plus, Var "x", Lit (StringVal "world"))))
  )

let test_ex3 () =
  let env = Env.update "b" (Base (Bool, Finite 0)) Env.empty in
  let env' = transform env ex3 in
  assert (Env.lookup "x" env' = Any (Finite 1));
  print_endline "Example 3: ✓"

(** Run all tests *)
let run_all () =
  test_ex1 ();
  test_ex2 ();
  test_ex3 ();
  print_endline "All examples passed!"
  