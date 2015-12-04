
module Lang = Fold_lang
module Eval = Fold_evaluator

open Elements

open Pratt
open Pratt.Foundation
open Pratt.Syntax
open Pratt.Lexer
open Pratt.Parser
open Pratt.Env

let fold_logo =
  blue ("     ▗▐▝        ▐▐      ▐▐   ") ^ ("|" ^ bright_white "  A modern pragmatic functional language.\n") ^
  blue ("    ▐▐     ▗▗   ▐▐    ▗▗▐▐   ") ^ ("|" ^ bright_white "\n") ^
  blue ("  ▝▝▐▐▝▝ ▐▐  ▐▐ ▐▐  ▐▐  ▐▐   ") ^ ("|" ^ bright_white "  Version 0.0.1-alpha+001 (2015-02-12)\n") ^
  blue ("    ▐▐   ▐▐  ▐▐ ▐▐  ▐▐  ▐▐   ") ^ ("|  " ^ (underline (bright_white "http://fold-lang.org\n"))) ^
  blue ("    ▐▐     ▝▝    ▝▝   ▝▝▝    ") ^ ("|" ^ bright_white "  Type \? for help.\n") ^
  blue ("  ▗▐▝                      \n") ^
  "                           \n"  ^
  (italic "  \"Simplicity is prerequisite for reliability.\"\n") ^
  "      — Edsger W. Dijkstra"


let rec loop env =
  try
    print_string (bright_blue "-> " ^ start_white); flush stdout;
    let lexer = create_lexer_with_channel "<REPL>" stdin in
    let expr = Pratt.init ~lexer ~grammar:Lang.core_lang in
    if expr = Atom (Sym "quit") then
      Eval.quit ();
    let (env, value) = Eval.eval env expr in
    if value <> Expr.unit then
      print (Eval.show_value value);
    loop env
  with
  | Failure msg
  | Invalid_argument msg ->
    print (bright_red " * Error" ^ ": " ^ msg);
    flush stdout;
    loop env
  | exn ->
    let msg = Exn.to_string exn in
    print (bright_red " * Error" ^ ": " ^ msg);
    flush stdout;
    loop env

let env =
  Env.empty
  |> Env.add "T" (Atom (Bool true))
  |> Env.add "F" (Atom (Bool false))

let () = begin
  print (end_color ^ "\n" ^ fold_logo ^ "\n");
  loop env
end

