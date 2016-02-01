
open Pratt_foundation
open Pratt_env
open Pratt_lexer

type 'a with_meta = { data : 'a; meta : expr option }
and expr =
  | Atom of literal             with_meta
  | List of (expr list)         with_meta
  | Func of (expr list -> expr) with_meta

module Show = struct
  include Show
  let rec exp = function
    | Atom { data = x  } -> show_literal x
    | List { data = xs } -> fmt "(%s)" @ String.concat " " @ List.map exp xs
    | Func { data = _  } -> "<λ>"
end

module Expr = struct

  module Base = struct
    type t = expr
    let compare = Pervasives.compare
  end

  include Base

  module Map = Map.Make(Base)

  let unit      = Atom { data = Unit;    meta = None }
  let bool x    = Atom { data = Bool x;  meta = None }
  let str x     = Atom { data = Str x;   meta = None }
  let int x     = Atom { data = Int x;   meta = None }
  let sym    x  = Atom { data = Sym x;   meta = None }
  let list xs   = List { data = xs;      meta = None }
  let func f    = Func { data = f;       meta = None }
  let call f xs = List { data = f :: xs; meta = None }

  let atom x    = Atom { data = x;       meta = None }
  let seq  x y  = call (sym ";") [x; y]

  let unwrap_str  = function Atom { data = Str x  } -> x | _ -> fail "expression is not a string"
  let unwrap_sym  = function Atom { data = Sym x  } -> x | _ -> fail "expression is not a symbol"
  let unwrap_int  = function Atom { data = Int x  } -> x | _ -> fail "expression is not an int"
  let unwrap_bool = function Atom { data = Bool x } -> x | _ -> fail "expression is not a bool"
end

