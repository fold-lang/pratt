
open Foundation
open Lexer

type expr =
  | Lit of literal           (* Basic literal values: sym, str, int, etc. *)
  | Seq of expr list         (* A sequence of expressions: `a b c d`.     *)
  | App of expr * expr list  (* Function applicaton: `f x`, `(f x) y`.    *)

let unit = Lit (Sym "()")

let rand_expr = Lit (Int 42)

let rec show_expr = function
  | Lit x -> show_literal x
  | App (f, xs) -> format "(app %s %s)" (show_expr f) (join " " (map show_expr xs))
  | Seq xs -> format "(seq %s)" (join " " (map show_expr xs))

let append_expr x y = match x with
  | App (f, xs) -> App (f, append xs [y])
  | Seq xs      -> Seq    (append xs [y])
  | Lit _       -> Seq [x; y]

