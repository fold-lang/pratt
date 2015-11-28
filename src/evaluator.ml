
open Syntax
open Lexer
open Grammar

let rec eval expr env =
  match expr with
  | Term (Sym "define", [name; body]) ->
    let value = (eval body env) in
    Env.set env key value;
    value
  | expr -> expr

