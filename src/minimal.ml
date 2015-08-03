(*
 * Minimal is a minimal ML implementation with Pratt parser.
 *)

let tokens = [
  "int";
  "bool";
  "true";
  "false";
  "fun";
  "is";
  "if";
  "then";
  "else";
  "let";
  ";;";
  "=";
  "<";
  "->";
  ":";
  "(";
  ")";
  "+";
  "-";
  "*";
  "/";
]

open Pratt
open Fold_parser
open Fold_grammar
open Fold_lexer
open Fold_syntax


let grammar =
  let open Scope in
  let rules =
    empty
    |> define (delimiter     (Sym "EOF"))
    (* |> define (newline      (Sym "EOL") 10) *)
    |> define (binary_infix (Sym "+")   50)
    |> define (binary_infix (Sym "-")   50)
    (* |> define (unary_prefix (Sym "-")   70) *)
    |> define (binary_infix (Sym "*")   60)
    |> define (binary_infix (Sym "/")   60)
    |> define (group        (Sym "(")
                            (Sym ")"))
  in
    grammar ~main:rules ~default

let () =
  print_endline "-- Minimal Demo Compiler";
  let result = Pratt.init
      ~lexer:(create_lexer_with_channel "<stdin>" stdin)
      ~grammar () in
  print_endline ("--\n" ^ (show_exp result))

