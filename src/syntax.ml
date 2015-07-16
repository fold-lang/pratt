
open Foundation
open Lexer

type exp =
  | Atom of literal          (* Basic atomic values: sym, str, int, etc. *)
  | List of exp list         (* A sequence of expressions: `a b c d`.    *)

let unit = Atom (Sym "()")

let rand_exp = Atom (Int 42)

let rec show_exp = function
  | Atom x -> show_literal x
  | List xs -> fmt "(%s)" (join " " (map show_exp xs))

