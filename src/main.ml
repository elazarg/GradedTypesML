(** Main driver for the graded type analyzer *)

open Graded_types
open Printf

let usage = "Usage: graded-typing [options] <file>"

type mode = 
  | Basic      (* Basic analysis *)
  | Ghost      (* With ghost state *)

let mode = ref Basic
let verbose = ref false
let input_file = ref ""

let spec = [
  ("-ghost", Arg.Unit (fun () -> mode := Ghost), 
   "Enable ghost state tracking");
  ("-v", Arg.Set verbose, "Verbose output");
]

(** Read file *)
let read_file filename =
  let ic = open_in filename in
  let rec read_all acc =
    try
      let line = input_line ic in
      read_all (acc ^ line ^ "\n")
    with End_of_file ->
      close_in ic;
      acc
  in
  read_all ""

(** Run basic analysis *)
let analyze_basic stmt =
  let open Interp in
  let env = Env.empty in
  if !verbose then printf "Initial: %s\n" (Env.pp env);
  let env' = transform env stmt in
  printf "Final: %s\n" (Env.pp env');
  env'

(** Run analysis with ghost state *)
let analyze_ghost stmt =
  let open Interp_ghost in
  let open Ghost in (* This brings the AugEnv module into scope *)
  let env = AugEnv.empty in
  if !verbose then printf "Initial: %s\n" (AugEnv.pp env);
  let env' = transform_ghost env stmt in
  printf "Final with blame: %s\n" (AugEnv.pp env');
  env'

(** Main entry point *)
let main () =
  Arg.parse spec (fun f -> input_file := f) usage;
  
  if !input_file = "" then begin
    eprintf "Error: no input file specified\n";
    exit 1
  end;
  
  try
    let input = read_file !input_file in
    let stmt = Parser.parse input in
    if !verbose then 
      printf "Parsed: %s\n\n" (Ast.pp_stmt stmt);
    
    match !mode with
    | Basic -> analyze_basic stmt |> ignore
    | Ghost -> analyze_ghost stmt |> ignore
    
  with
  | Parser.Parse_error msg ->
      eprintf "Parse error: %s\n" msg;
      exit 1
  | Sys_error msg ->
      eprintf "Error: %s\n" msg;
      exit 1
  | e ->
      eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1

let () = main ()
