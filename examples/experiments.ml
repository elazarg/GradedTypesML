(** Experiments: where graded typing gives results that are
    demonstrably better than gradual typing ("just dyn")
    and more flexible than simple typing ("reject on mismatch"). *)

open Graded_types
open Ast
open Types

(* ================================================================
   Helper: run analysis on a statement, print the result environment
   ================================================================ *)
let analyze ?(init=Env.empty) name stmt =
  let env' = Interp.transform init stmt in
  Printf.printf "%-50s => %s\n" name (Env.pp env');
  env'

let sep title =
  Printf.printf "\n=== %s ===\n\n" title

(* ================================================================
   Side-by-side comparison: graded vs gradual
   ================================================================ *)
let compare ?(init=Env.empty) name stmt =
  let graded  = Interp.transform init stmt in
  let gradual = Interp_gradual.transform init stmt in
  Printf.printf "  %-44s  graded:  %s\n" name (Env.pp graded);
  Printf.printf "  %-44s  gradual: %s\n" "" (Env.pp gradual)

let bool_env =
  Env.update "b" (Base (Bool, Finite 0))
    (Env.update "c" (Base (Bool, Finite 0)) Env.empty)

(* ================================================================
   EXAMPLE 1: Homogeneous branch — grade preserves precision
   ================================================================

   Simple typing:  x: Int  (OK)
   Gradual typing: x: dyn  (conservative if flow-insensitive, Int if flow-sensitive)
   Graded typing:  x: Int^0  (precise — grade 0 certifies ZERO merges lost info)
*)
let ex_homo_branch =
  If (Var "b",
      Assign ("x", Lit (IntVal 1)),
      Assign ("x", Lit (IntVal 2)))

(* ================================================================
   EXAMPLE 2: Heterogeneous branch — the motivating case
   ================================================================

   Simple typing:  REJECTED  (type mismatch: Int vs String)
   Gradual typing: x: dyn
   Graded typing:  x: Any^0  (accepted, grade 0 = one merge, no coercions yet)
*)
let ex_hetero_branch =
  If (Var "b",
      Assign ("x", Lit (IntVal 1)),
      Assign ("x", Lit (StringVal "hello")))

(* ================================================================
   EXAMPLE 3: Arithmetic on known types — grades stay precise
   ================================================================

   x := 1; y := x + 2; z := y + 3

   Simple typing:  z: Int  (OK)
   Gradual typing: z: Int  (OK)
   Graded typing:  z: Int^0  (grade 0 = the entire pipeline is precise)

   Under the OLD semantics (Plus always joins Int/String interps):
     z would be Any^1 — spurious imprecision.
   Under the NEW semantics (prefer compatible interpretations):
     z is Int^0 — correct!
*)
let ex_precise_pipeline =
  Seq (Assign ("x", Lit (IntVal 1)),
  Seq (Assign ("y", Binop (Plus, Var "x", Lit (IntVal 2))),
       Assign ("z", Binop (Plus, Var "y", Lit (IntVal 3)))))

(* ================================================================
   EXAMPLE 4: Imprecision enters once, then tracked downstream
   ================================================================

   if b then x := 1 else x := "hi"     // x: Any^0
   y := x + 1                           // y: Int^1  (one coercion needed)
   z := y + 1                           // z: Int^1  (y is already Int — free!)

   Simple typing:  REJECTED at the if-merge
   Gradual typing: y: dyn, z: dyn  (no distinction from x)
                   OR y: Int, z: Int  (if casts inserted — but no confidence info)
   Graded typing:  x: Any^0, y: Int^1, z: Int^1

   KEY INSIGHT: grade 1 "sticks" but does NOT grow. Once a value is
   committed to Int (even speculatively), subsequent Int operations
   add no further imprecision. The grade is a cumulative record, not
   a per-operation tax.
*)
let ex_grade_sticks =
  Seq (
    If (Var "b",
        Assign ("x", Lit (IntVal 1)),
        Assign ("x", Lit (StringVal "hi"))),
  Seq (
    Assign ("y", Binop (Plus, Var "x", Lit (IntVal 1))),
    Assign ("z", Binop (Plus, Var "y", Lit (IntVal 1)))))

(* ================================================================
   EXAMPLE 5: Two pipelines with different confidence levels
   ================================================================

   // Pipeline A — fully type-safe
   a := 1; a := a + 2; a := a + 3      // a: Int^0 — perfect

   // Pipeline B — one imprecise input
   if b then b_ := 1 else b_ := "hi"   // b_: Any^0
   b_ := b_ + 2                        // b_: Int^1
   b_ := b_ + 3                        // b_: Int^1

   Simple typing:  Pipeline A: OK.  Pipeline B: REJECTED.
   Gradual typing: a: Int, b_: Int (same — no distinction!)
   Graded typing:  a: Int^0, b_: Int^1

   The grade DISTINGUISHES the two pipelines. A compiler/linter can:
   - Pipeline A (grade 0): safely unbox, optimize, skip runtime checks
   - Pipeline B (grade 1): one speculation happened — insert a check
     at the coercion site, not everywhere
*)
let ex_two_pipelines =
  Seq (
    (* Pipeline A *)
    Seq (Assign ("a", Lit (IntVal 1)),
    Seq (Assign ("a", Binop (Plus, Var "a", Lit (IntVal 2))),
         Assign ("a", Binop (Plus, Var "a", Lit (IntVal 3))))),
    (* Pipeline B *)
    Seq (
      If (Var "b",
          Assign ("b_", Lit (IntVal 1)),
          Assign ("b_", Lit (StringVal "hi"))),
    Seq (Assign ("b_", Binop (Plus, Var "b_", Lit (IntVal 2))),
         Assign ("b_", Binop (Plus, Var "b_", Lit (IntVal 3))))))

(* ================================================================
   EXAMPLE 6: Increasing imprecision through re-merges
   ================================================================

   if a then x := 1 else x := "hi"       // x: Any^0
   y := x + 1                            // y: Int^1
   if b then y := y else y := "world"    // y: Any^1  (re-merge at grade 1)
   z := y + 1                            // z: Int^2  (two speculations deep)

   Simple typing:  REJECTED
   Gradual typing: z: dyn (or Int after cast — still no quantification)
   Graded typing:  z: Int^2

   Grade 2 tells you: TWO speculative steps are embedded in this value.
   This is a static "risk score" — higher grade = more fragile.
   Gradual typing has no analog; everything uncertain is equally dyn.
*)
let ex_grade_accumulates =
  Seq (
    If (Var "b",
        Assign ("x", Lit (IntVal 1)),
        Assign ("x", Lit (StringVal "hi"))),
  Seq (
    Assign ("y", Binop (Plus, Var "x", Lit (IntVal 1))),
  Seq (
    If (Var "c",
        Assign ("y", Var "y"),
        Assign ("y", Lit (StringVal "world"))),
    Assign ("z", Binop (Plus, Var "y", Lit (IntVal 1))))))

(* ================================================================
   EXAMPLE 7: Type-stable loop — fixpoint at grade 0
   ================================================================

   x := 0; while b do x := x + 1

   Simple typing:  x: Int  (OK — types consistent through loop)
   Gradual typing: x: dyn  (conservative) or Int (if flow-sensitive)
   Graded typing:  x: Int^0  (fixpoint reached IMMEDIATELY — Int+Int=Int)

   With the old Plus semantics, x+1 would produce Any^1, and the
   loop fixpoint would diverge. With the fixed Plus, the loop body
   preserves types exactly, so the fixpoint is reached in one step.
*)
let ex_stable_loop =
  Seq (
    Assign ("x", Lit (IntVal 0)),
    While (Var "b",
           Assign ("x", Binop (Plus, Var "x", Lit (IntVal 1)))))

(* ================================================================
   EXAMPLE 8: Operator directs the coercion — no spurious ambiguity
   ================================================================

   x := 1
   y := x && true

   AND always requires Bool. So coercing Int^0 to Bool costs +1:
   y: Bool^1.

   But: x := true; y := x && false → y: Bool^0. No coercion needed.

   The grade distinguishes "was this value naturally boolean?" from
   "was it cast to boolean?". This is information gradual typing
   doesn't track.
*)
let ex_bool_coercion =
  Seq (Assign ("x", Lit (IntVal 1)),
       Assign ("y", Binop (And, Var "x", Lit (BoolVal true))))

let ex_bool_natural =
  Seq (Assign ("x", Lit (BoolVal true)),
       Assign ("y", Binop (And, Var "x", Lit (BoolVal false))))

(* ================================================================
   EXAMPLE 9: Correlated branches (from paper) — cost-count helps
   ================================================================

   if b then x := 1 else x := "hello"
   if b then x := x + 2 else x := x + "world"

   The second if has correlated branches: branch 1 uses x as Int,
   branch 2 uses x as String. Each branch's Plus now picks the
   compatible interpretation:
     Branch 1: x(Any^0) + 2(Int^0)  → Int^1  (Int preferred, cost 1 < 2)
     Branch 2: x(Any^0) + "world"   → String^1 (String preferred, cost 1 < 2)
   Join: Any^1

   Same result as old semantics for this example, but each branch
   is now individually more precise (Int^1 or String^1, not Any^1).
*)
let ex_correlated =
  Seq (
    If (Var "b",
        Assign ("x", Lit (IntVal 5)),
        Assign ("x", Lit (StringVal "hello"))),
    If (Var "b",
        Assign ("x", Binop (Plus, Var "x", Lit (IntVal 2))),
        Assign ("x", Binop (Plus, Var "x", Lit (StringVal "world")))))

(* ================================================================
   EXAMPLE 10: Mixed — grade as a code quality metric
   ================================================================

   // Good function: all types consistent
   a := 1; a := a + a; a := a + a       // a: Int^0

   // OK function: one merge, then used consistently
   if b then d := 1 else d := "x"       // d: Any^0
   d := d + 1                           // d: Int^1

   // Bad function: repeated re-merges
   if b then e := 1 else e := "x"       // e: Any^0
   e := e + 1                           // e: Int^1
   if c then e := e else e := "y"       // e: Any^1
   e := e + 1                           // e: Int^2

   Grade summary:  a=Int^0, d=Int^1, e=Int^2
   → This IS a code quality metric. Higher grade = more brittle.
   Gradual typing gives no such ranking.
*)
let ex_quality_metric =
  Seq (
    (* Good: grade 0 *)
    Seq (Assign ("a", Lit (IntVal 1)),
    Seq (Assign ("a", Binop (Plus, Var "a", Var "a")),
         Assign ("a", Binop (Plus, Var "a", Var "a")))),
  Seq (
    (* OK: grade 1 *)
    Seq (
      If (Var "b",
          Assign ("d", Lit (IntVal 1)),
          Assign ("d", Lit (StringVal "x"))),
      Assign ("d", Binop (Plus, Var "d", Lit (IntVal 1)))),
    (* Bad: grade 2 *)
    Seq (
      If (Var "b",
          Assign ("e", Lit (IntVal 1)),
          Assign ("e", Lit (StringVal "x"))),
    Seq (
      Assign ("e", Binop (Plus, Var "e", Lit (IntVal 1))),
    Seq (
      If (Var "c",
          Assign ("e", Var "e"),
          Assign ("e", Lit (StringVal "y"))),
      Assign ("e", Binop (Plus, Var "e", Lit (IntVal 1))))))))

(* ================================================================
   RUN ALL
   ================================================================ *)
let () =
  sep "1. Homogeneous branch: grade preserves precision";
  ignore (analyze ~init:bool_env "if b then x:=1 else x:=2" ex_homo_branch);

  sep "2. Heterogeneous branch: accepted with grade 0";
  ignore (analyze ~init:bool_env "if b then x:=1 else x:=\"hello\"" ex_hetero_branch);

  sep "3. Precise pipeline: Int+Int stays Int^0";
  ignore (analyze "x:=1; y:=x+2; z:=y+3" ex_precise_pipeline);

  sep "4. Imprecision enters once, grade sticks but doesn't grow";
  ignore (analyze ~init:bool_env
    "if b then x:=1 else x:=\"hi\"; y:=x+1; z:=y+1" ex_grade_sticks);

  sep "5. Two pipelines: grade distinguishes confidence";
  ignore (analyze ~init:bool_env
    "pipeline A (type-safe) vs B (one merge)" ex_two_pipelines);

  sep "6. Re-merges: grade accumulates (Int^2)";
  ignore (analyze ~init:bool_env
    "x:=Any^0 -> y:=Int^1 -> re-merge -> z:=Int^2" ex_grade_accumulates);

  sep "7. Type-stable loop: fixpoint at grade 0";
  ignore (analyze ~init:bool_env "x:=0; while b do x:=x+1" ex_stable_loop);

  sep "8. Bool coercion: grade tracks implicit casts";
  ignore (analyze "int cast to bool: y=Bool^1" ex_bool_coercion);
  ignore (analyze "bool used as bool: y=Bool^0" ex_bool_natural);

  sep "9. Correlated branches (paper example 3)";
  ignore (analyze ~init:bool_env
    "correlated: x:=Any^1" ex_correlated);

  sep "10. Grade as code quality metric: 0=good, 1=ok, 2=bad";
  ignore (analyze ~init:bool_env
    "a:=Int^0, d:=Int^1, e:=Int^2" ex_quality_metric);

  sep "SUMMARY";
  print_endline "Graded typing gives three things no other system gives simultaneously:";
  print_endline "";
  print_endline "1. ACCEPTS programs simple typing rejects (like gradual typing)";
  print_endline "2. QUANTIFIES imprecision (unlike gradual typing's binary dyn/concrete)";
  print_endline "3. Grade is STICKY but NOT compounding — once committed to a type,";
  print_endline "   further operations at that type are FREE (grade doesn't grow)";
  print_endline "";
  print_endline "The grade is a static 'confidence score' / 'risk metric':";
  print_endline "  Int^0 = definitely Int, safe to optimize";
  print_endline "  Int^1 = probably Int, one speculation — insert one runtime check";
  print_endline "  Int^2 = two speculations deep — more fragile, investigate";
  print_endline "  Any^k = type unknown, k speculations accumulated";

  (* ================================================================
     GRADED vs GRADUAL: side-by-side comparison
     ================================================================ *)
  sep "GRADED vs GRADUAL: side-by-side comparison";
  print_endline "Same program, two analyses. Differences show what graded typing gains.\n";

  print_endline "--- Homogeneous branch (no imprecision) ---";
  compare ~init:bool_env "if b then x:=1 else x:=2" ex_homo_branch;
  print_endline "  Both agree: no coercion, no difference.\n";

  print_endline "--- One coercion, then used downstream ---";
  compare ~init:bool_env
    "if b then x:=1 else x:=\"hi\"; y:=x+1; z:=y+1" ex_grade_sticks;
  print_endline "  Graded: y,z at Int^1. Gradual: y,z at Int^inf.\n";

  print_endline "--- Two pipelines: safe vs one merge ---";
  compare ~init:bool_env
    "pipeline A vs B" ex_two_pipelines;
  print_endline "  Graded distinguishes a:Int^0 from b_:Int^1.\n";

  print_endline "--- Re-merge chain (grade accumulates) ---";
  compare ~init:bool_env
    "x->Any^0, y->Int^1, re-merge, z->Int^2" ex_grade_accumulates;
  print_endline "  Graded: z at Int^2. Gradual: z at Int^inf.\n";

  print_endline "--- Quality metric: three confidence levels ---";
  compare ~init:bool_env
    "a:good, d:ok, e:bad" ex_quality_metric;
  print_endline "  Graded: Int^0/Int^1/Int^2. Gradual: Int^0/Int^inf/Int^inf.\n";

  print_endline "--- Type-stable loop ---";
  compare ~init:bool_env
    "x:=0; while b do x:=x+1" ex_stable_loop;
  print_endline "  Both agree: loop body preserves types, fixpoint in 1 step.\n";

  sep "CONCLUSION";
  print_endline "Gradual typing is graded typing on the collapsed lattice {Bot, 0, inf}.";
  print_endline "The ONLY difference is succ_grade:";
  print_endline "  Graded:  Finite n -> Finite (n+1)";
  print_endline "  Gradual: Finite n -> Inf";
  print_endline "";
  print_endline "Where graded typing distinguishes Int^1 from Int^2 (one vs two";
  print_endline "speculations), gradual typing collapses both to Int^inf (= dyn).";
  ()
