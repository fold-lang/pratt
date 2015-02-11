
open Foundation
open Lexicon

type expr = Atom of literal
          | Term of literal * expr list

let rec show_expr = function
    | Atom x -> show_literal x
    | Term (f, args) -> format "(%s: %s)" (show_literal f) (join " " (map show_expr args))


