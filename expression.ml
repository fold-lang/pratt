
type t =
    | Add of t * t
    | Mul of t * t
    | Sub of t * t
    | Int of int
    | Var of string


let rec eval env = function
    | Int a -> a
    | Var a -> env a
    | Mul (a, b) -> (eval env a) * (eval env b)
    | Add (a, b) -> (eval env a) + (eval env b)
    | Sub (a, b) -> (eval env a) - (eval env b)


let rec to_string expr =
  match expr with
  | Add (e1, e2) ->
      Printf.sprintf "(%s + %s)" (to_string e1) (to_string e2)
  | Mul (e1, e2) ->
      Printf.sprintf "(%s * %s)" (to_string e1) (to_string e2)
  | Sub (e1, e2) ->
      Printf.sprintf "(%s - %s)" (to_string e1) (to_string e2)
  | Int x -> string_of_int x
  | Var x -> x


let peek_expr e = Foundation.print (to_string e); e

