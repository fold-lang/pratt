
open Foundation
open Fold_lexer

type exp =
  | Atom of literal          (* Basic atomic values: sym, str, int, etc. *)
  | List of exp list         (* A sequence of expressions: `a b c d`.    *)

let rec show_exp = function
  | Atom x -> show_literal x
  | List xs -> fmt "(%s)" (join " " (map show_exp xs))

