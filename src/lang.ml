
open Foundation
open Parser
open Pratt
open Syntax
open Lexer
open Grammar


(* TODO:
   - Prefix symbols must be created by juxtaposition and not explicitly.
   - Improve the parsing of recursive prefix expressions (eg: "f f x").
 *)
let core_lang =
  let open Scope in
  let main_scope =
    empty
    |> define (terminal           (Sym "EOF"))
    |> define (newline            (Sym "EOL") 10)
    |> define (binary_infix       (Sym "+")   50)
    |> define (binary_infix       (Sym "-")   50)
    |> define (binary_infix       (Sym "#")   30)
    |> define (binary_infix       (Sym "*")   60)
    |> define (binary_infix       (Sym "=")   10)
    |> define (binary_infix_right (Sym ";")   20)
    |> define (closed (Sym "(") (Sym ")"))
    (* |> Scope.define (closed (Sym "{") (Sym "}")) *)
    (* |> Scope.define (unary_prefix       (Sym "f")   70) *)
    (* |> Scope.define (unary_prefix       (Sym "-")   70) *)
  in grammar ~main: main_scope ~default

