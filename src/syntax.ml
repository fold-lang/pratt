
open Foundation
open Lexer

type expr = Atom of literal
          | Term of literal * expr list

let epsilon = Atom (Symbol "")

let rec show_expr = function
    | Atom x -> show_literal x
    | Term (f, args) -> format "(%s : %s)" (show_literal f) (join " " (map show_expr args))


let append_expr e1 e2 =
	e1 => function | Term (head, args) -> Term (head, args @ [e2])
				   | Atom head -> print "are you sure?"; Term (head, [e2])
