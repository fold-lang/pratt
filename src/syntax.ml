
open Foundation
open Lexer

type expr =
  | Atom of literal              (* Basic atomic values: sym, str, int, etc.       *)
  | Term of (expr * expr list)   (* Expression product (head, args): `(f: x y z)`. *)

let rec show_expr = function
  | Atom x       -> show_literal x
  | Term (f, xs) -> fmt "(%s: %s)" (show_expr f) (join " " (map show_expr xs))

let atom  x     = Atom x
let str   x     = Atom (Str x)
let int   x     = Atom (Int x)
let sym   x     = Atom (Sym x)
let seq   x  y  = Term (Atom (Sym ";")   , [x; y])
let list  xs    = Term (Atom (Sym "list"), xs)
let dict  xs    = Term (Atom (Sym "dict"), xs)
let fn    s  xs = Term (s, xs)


