(** Simple S-expression parser for the while language *)

open Ast

type sexp =
  | Atom of string
  | List of sexp list

exception Parse_error of string

(** Tokenizer *)
let tokenize input =
  let rec tokenize_rec i acc current =
    if i >= String.length input then
      if current = "" then List.rev acc
      else List.rev (current :: acc)
    else
      match input.[i] with
      | '(' -> 
          let acc' = if current = "" then acc else current :: acc in
          tokenize_rec (i+1) ("(" :: acc') ""
      | ')' ->
          let acc' = if current = "" then acc else current :: acc in
          tokenize_rec (i+1) (")" :: acc') ""
      | ' ' | '\t' | '\n' ->
          let acc' = if current = "" then acc else current :: acc in
          tokenize_rec (i+1) acc' ""
      | c ->
          tokenize_rec (i+1) acc (current ^ String.make 1 c)
  in
  tokenize_rec 0 [] ""

(** Parse S-expressions *)
let parse_sexp tokens =
  let rec parse_rec tokens =
    match tokens with
    | [] -> raise (Parse_error "unexpected end of input")
    | "(" :: rest ->
        let rec collect rest acc =
          match rest with
          | ")" :: rest' -> (List (List.rev acc), rest')
          | _ ->
              let (sexp, rest') = parse_rec rest in
              collect rest' (sexp :: acc)
        in
        collect rest []
    | ")" :: _ -> raise (Parse_error "unexpected )")
    | atom :: rest -> (Atom atom, rest)
  in
  let (sexp, rest) = parse_rec tokens in
  if rest <> [] then raise (Parse_error "trailing input");
  sexp

(** Parse expressions *)
let rec parse_expr = function
  | Atom s ->
      (try Lit (IntVal (int_of_string s))
       with _ ->
         if s = "true" then Lit (BoolVal true)
         else if s = "false" then Lit (BoolVal false)
         else if String.length s > 0 && s.[0] = '"' then
           Lit (StringVal (String.sub s 1 (String.length s - 2)))
         else Var s)
  | List [Atom "+"; e1; e2] ->
      Binop (Plus, parse_expr e1, parse_expr e2)
  | List [Atom "&&"; e1; e2] ->
      Binop (And, parse_expr e1, parse_expr e2)
  | List [Atom "="; e1; e2] ->
      Binop (Eq, parse_expr e1, parse_expr e2)
  | List [Atom "not"; e] ->
      Not (parse_expr e)
  | _ -> raise (Parse_error "invalid expression")

(** Parse statements *)
let rec parse_stmt = function
  | Atom "skip" -> Skip
  | List [Atom ":="; Atom x; e] ->
      Assign (x, parse_expr e)
  | List [Atom "seq"; s1; s2] ->
      Seq (parse_stmt s1, parse_stmt s2)
  | List [Atom "if"; e; s1; s2] ->
      If (parse_expr e, parse_stmt s1, parse_stmt s2)
  | List [Atom "while"; e; s] ->
      While (parse_expr e, parse_stmt s)
  | _ -> raise (Parse_error "invalid statement")

(** Main parser *)
let parse input =
  let tokens = tokenize input in
  let sexp = parse_sexp tokens in
  parse_stmt sexp

(** Examples of concrete syntax:
    skip
    (:= x 5)
    (seq (:= x 5) (:= y 10))
    (if b (:= x 1) (:= x "hello"))
    (while b (:= x (+ x 1)))
*)
