(* module Lex = Pratt.Lexer *)

(* module Expr = struct *)
  (* module Base = struct *)
    (* type t = *)
      (* | Atom of Lex.literal   with_meta *)
      (* | List of (t list)      with_meta *)
      (* | Func of (t list -> t) with_meta *)

     (* and 'a with_meta = *)
      (* { value : 'a; meta : t } *)

    (* let compare = Pervasives.compare *)
  (* end *)

  (* include Base *)

  (* module Map = Map.Make(Base) *)
(* end *)


open Pratt_foundation
open Pratt_lexer

type expr =
  | Atom of literal
  | List of expr list

module Expr = struct

  module Base = struct
    type t = expr
    let compare = Pervasives.compare
  end

  include Base

  module Map = Map.Make(Base)

  let rec show = function
    | Atom x  -> show_literal x
    | List xs -> fmt "(%s)" (join " " (map show xs))

  let atom  x     = Atom x
  let str   x     = Atom (Str x)
  let int   x     = Atom (Int x)
  let sym   x     = Atom (Sym x)
  let seq   x  y  = List [sym ";"; x; y]
  let list  xs    = List xs
  let dict  xs    = List [sym "dict"; xs]
  let fn    f  xs = List (f :: xs)
end


