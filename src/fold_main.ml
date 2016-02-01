
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
  blue ("    ▐▐     ▝▝    ▝▝   ▝▝▝    ") ^ ("|" ^ bright_white "  Type \\? for help.\n") ^
  blue ("  ▗▐▝                      \n") ^
  "                           \n"  ^
  (italic "  \"Simplicity is prerequisite for reliability.\"\n") ^
  "      — Edsger W. Dijkstra"


let rec loop env =
  try
    print_string (bright_blue "-> " ^ start_white); flush stdout;
    let lexer = create_lexer_with_channel "<REPL>" stdin in
    let exp = Pratt.init ~lexer ~grammar:Lang.core_lang in
    print @ fmt ">> %s" @ Show.exp exp;
    let (env, value) = Eval.eval env exp in
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

let factorial i =
  let rec loop acc i =
    if i = 0 then acc
    else loop (i * acc) (i - 1) in
  loop 1 i


let env =
  Env.empty
  |> Env.add "T"     (Expr.atom (Bool true))
  |> Env.add "F"     (Expr.atom (Bool false))
  |> Env.add "list"  (Expr.func (fun xs -> Expr.list xs))
  |> Env.add "+"     (Fold_evaluator.bin_numeric_function ( + ))
  |> Env.add "-"     (Fold_evaluator.bin_numeric_function ( - ))
  |> Env.add "/"     (Fold_evaluator.bin_numeric_function ( / ))
  |> Env.add "*"     (Fold_evaluator.bin_numeric_function ( * ))
  |> Env.add "!"     (Fold_evaluator.unary_numeric_function factorial)
  |> Env.add "cons"  Fold_evaluator.cons
  |> Env.add "table" Fold_evaluator.table

let () = begin
  print (end_color ^ "\n" ^ fold_logo ^ "\n");
  loop env
end

