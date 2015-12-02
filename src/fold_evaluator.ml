
module Env  = Fold_env

open Elements
open Pratt.Syntax
open Pratt.Lexer
open Pratt.Grammar


let rec quasiquote expr =
  Log.inf "Will perform quasiquote interpolation...";
  match expr with
  | List [Atom (Sym "%"); expr] -> expr
  | List (x :: xs) -> List [quasiquote x; quasiquote (List xs)]
  | List (x :: xs) -> List [Atom (Sym "cons"); quasiquote x; quasiquote (List xs)]
  | expr -> List [Atom (Sym "simple_quote"); expr]

let rec eval env expr =
  match expr with
  (* Atoms are evaluate to themselves. *)
  | Atom (Sym s) -> Env.get env expr

  | Atom x -> expr

  | List [Atom (Sym "define"); key; body] ->
    let value = (eval env body) in
    Env.set env key value;
    value

  | List (Atom (Sym "+") :: args) ->
    let args = List.map args ~f:(eval env) in
    List.fold args ~init:(Expr.int 0) ~f:begin fun a b ->
      match a, b with
      | Atom (Int a), Atom (Int b) -> Atom (Int (a + b))
      | a, b -> invalid_arg (fmt "Numeric args required for `+, %s and %s given."
                               (Expr.show a) (Expr.show b))
    end

  | List [Atom (Sym "`"); expr]
  | List [Atom (Sym "quote"); expr]
  | List [Atom (Sym "quasiquote"); expr] -> eval env (quasiquote expr)
  | List [Atom (Sym "simple_quote"); expr] -> expr

  | List xs ->
    let expr = List (List.map xs ~f:(eval env)) in
    eval env expr

