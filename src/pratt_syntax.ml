
open Pratt_foundation
open Pratt_env
open Pratt_lexer


type expr =
  | Atom of literal
  | List of expr list
  | Func of expr list * expr * expr env
  (* | Func of expr list * expr env -> expr *)

module Expr = struct

  module Base = struct
    type t = expr
    let compare = Pervasives.compare
  end

  include Base

  module Map = Map.Make(Base)

  let rec show = function
    | Atom ((Sym _) as x)  -> "`" ^ show_literal x
    | Atom x               -> show_literal x
    | List xs              -> fmt "(%s)" (join " " (map show xs))
    | Func (args, body, _) -> fmt "(λ (%s) %s)" (join " " (map show args)) (show body)

  let atom  x      = Atom x
  let unit         = Atom Unit
  let str   x      = Atom (Str x)
  let int   x      = Atom (Int x)
  let sym   x      = Atom (Sym x)
  let seq   x  y   = List [sym ";"; x; y]
  let list  xs     = List xs
  let dict  xs     = List [sym "dict"; xs]
  let func  args x = Func (args, x, Env.empty)
  let call  f args = List (f :: args)

  let unwrap_str = function (Atom (Str x)) -> x | _ -> fail "expression is not a string"
  let unwrap_sym = function (Atom (Sym x)) -> x | _ -> fail "expression is not a symbol"
  let unwrap_int = function (Atom (Int x)) -> x | _ -> fail "expression is not an int"
end


