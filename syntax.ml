
open Foundation
open Lexicon

type expr =
    | List of expr list
    | Atom of literal

let empty_expr = List []

let append_expr e1 e2 = (e1, e2) => function
    | List l1, List l2 -> List (l1 @ l2)
    | e, List l -> List (e::l)
    | List l, e -> List (l @ [e])
    | _, _ -> List [e1; e2]

let rec show_expr = function
    | Atom x -> show_literal x
    | List l -> format "(%s)" (join " " (map show_expr l))

