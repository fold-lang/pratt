
type t =
    | Add of t * t
    | Mul of t * t
    | Sub of t * t
    | Assignment of t * t
    | Int of int
    | Not of t
    | End
    | Start of t
    | Var of string


let rec show expr =
  match expr with
  | Add (e1, e2) ->
      Printf.sprintf "(+ %s %s)" (show e1) (show e2)
  | Mul (e1, e2) ->
      Printf.sprintf "(* %s %s)" (show e1) (show e2)
  | Sub (e1, e2) ->
      Printf.sprintf "(- %s %s)" (show e1) (show e2)
  | Int x -> string_of_int x
  | Not x -> "(- " ^ (show x) ^ ")"
  | Var x -> "(var " ^ x  ^ ")"
  | End -> "(end)"
  | Start x -> "(root " ^ (show x) ^ ")"
  | Assignment (a, x) -> Printf.sprintf "(= %s %s)" (show a) (show x)


let peek_expr e = Foundation.print (show e); e

