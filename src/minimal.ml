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

open Parser
open Pratt
open Grammar
open Lexer
open Syntax


let grammar =
  let open Scope in
  let rules =
    empty
    |> define (terminal     (Sym "EOF"))
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
      ~lexer:(Lexer.create_lexer_with_channel "<stdin>" stdin)
      ~grammar () in
  print_endline ("--\n" ^ (Syntax.show_exp result))

