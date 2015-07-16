
open Foundation
open Parser
open Pratt
open Syntax
open Lexer
open Grammar

(* ASSIGNMENT  = 1; *)
(* CONDITIONAL = 2; *)
(* SUM         = 3; *)
(* PRODUCT     = 4; *)
(* EXPONENT    = 5; *)
(* PREFIX      = 6; *)
(* POSTFIX     = 7; *)
(* CALL        = 8; *)

let core_lang =
  let open Scope in
  let main_scope =
    empty
    |> define (terminal           (Sym "EOF"))
    |> define (binary_infix       (Sym "+")   30)
    |> define (binary_infix       (Sym "-")   30)
    |> define (binary_infix       (Sym "#")   20)
    |> define (binary_infix       (Sym "*")   40)
    |> define (binary_infix       (Sym "=")   10)
    |> define (binary_infix_right (Sym ";")   20)
    |> define (delimiter          (Sym ")"))
    |> define (group              (Sym "(") (Sym ")"))
    (* |> define (newline            (Sym "EOL") 10) *)
    (* |> define (closed (Sym "(") (Sym ")")) *)
    (* |> Scope.define (closed (Sym "{") (Sym "}")) *)
    (* |> Scope.define (unary_prefix       (Sym "f")   70) *)
    (* |> Scope.define (unary_prefix       (Sym "-")   70) *)
  in grammar ~main: main_scope ~default

