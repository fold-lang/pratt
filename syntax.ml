
open Foundation
open Lexicon

type expr = Atom of literal
          | Term of literal * expr list

let rec show_expr = function
    | Atom x -> show_literal x
    | Term (f, args) -> format "(%s: %s)" (show_literal f) (join " " (map show_expr args))

let empty_expr = Term (Symbol "", [])

let append_expr e1 e2 =
    (* print (format "append %s %s" (show_expr e1) (show_expr e2)); *)
    (e1, e2) => function
    | Term (f, args), _ -> Term (f, args @ [e2])
    | Atom a, Atom b -> Term (a, [e2])
    | _ -> raise (Failure (format "Cannot append: %s %s" (show_expr e1) (show_expr e2)))

