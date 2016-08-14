
open Pratt_foundation
open Pure
open Pratt_env
open Pratt_lexer


type value =
  | Atom of literal
  | Form of (expr list)
  | Func of (expr list -> expr)
  [@@deriving show]

and expr = { value : value; meta : (literal, literal) Assoc.t }
  [@@deriving show]

module Show = struct
  include Show
  let rec expr = function
    | { value = Atom x  } -> show_literal x

    | { value = Form [{ value = Atom (Sym ",") }; x; y] } ->
      "(%s, %s)" % (expr x, expr y)

    | { value = Form xs } ->
      "(%s)" % String.concat " " (List.map expr xs)

    | { value = Func _  } -> "<λ>"
end

module Expr = struct

  module Base = struct
    type t = expr
    let compare = Pervasives.compare
  end

  include Base

  module Map = Map.Make(Base)

  let atom x    = { value = Atom x         ; meta = [] }
  let unit   () = { value = Atom Unit      ; meta = [] }
  let bool x    = { value = Atom (Bool x)  ; meta = [] }
  let str  x    = { value = Atom (Str x)   ; meta = [] }
  let int  x    = { value = Atom (Int x)   ; meta = [] }
  let sym  x    = { value = Atom (Sym x)   ; meta = [] }
  let list   xs = { value = Form xs        ; meta = [] }
  let form   xs = { value = Form xs        ; meta = [] }
  let func f    = { value = Func f         ; meta = [] }
  let call f xs = { value = Form (f :: xs) ; meta = [] }

  let meta x m  = call (sym "meta") [x; m]
  let pair x y  = call (sym ",")    [x; y]
  let seq  x y  = call (sym ";")    [x; y]

  let unwrap_str  = function { value = Atom Str  x } -> x | _ -> fail "not a string"
  let unwrap_sym  = function { value = Atom Sym  x } -> x | _ -> fail "not a symbol"
  let unwrap_int  = function { value = Atom Int  x } -> x | _ -> fail "not an int"
  let unwrap_bool = function { value = Atom Bool x } -> x | _ -> fail "not a bool"
  let unwrap_func = function { value = Func      f } -> f | _ -> fail "not a function"

  let is_func = function { value = Func _ } -> true | _ -> false

  let append x y =
    match x with
    | { value = Form xs } -> form (List.append xs [y])
    | atom                -> form [atom; y]

end

