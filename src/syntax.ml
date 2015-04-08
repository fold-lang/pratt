
open Foundation
open Lexer

type expr = Atom of literal
          | Term of literal * expr list

let unit = Atom (Symbol "()")

let rand_expr = Atom (Integer 42)

let rec show_expr = function
    | Atom x -> show_literal x
    | Term (f, args) -> format "(%s : %s)" (show_literal f) (join " " (map show_expr args))


let append_expr x y =
  x => function | Term (head, args) -> Term (head, args @ [y])
                | _ -> raise (Failure "expression accepts no arguments")

let append_expr_list x ys =
  x => function | Term (head, args) -> Term (head, args @ ys)
                | _ -> raise (Failure "expression accepts no arguments")

