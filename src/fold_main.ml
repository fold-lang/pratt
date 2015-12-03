
module Lang = Fold_lang
module Eval = Fold_evaluator
module Env  = Fold_env

open Elements

open Pratt
open Pratt.Foundation
open Pratt.Syntax
open Pratt.Lexer
open Pratt.Parser

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

let loop () =
  let env   = Env.make None in
  Env.set env (Atom (Sym "T")) (Atom (Bool true));
  Env.set env (Atom (Sym "F")) (Atom (Bool false));
  while true do try
      print_string (bright_blue "-> " ^ start_white); flush stdout;
      let lexer = create_lexer_with_channel "<REPL>" stdin in
      let expr  = Pratt.init ~lexer ~grammar:Lang.core_lang in
      (if expr = Atom (Sym "quit") then Eval.quit ());
      let value = Eval.eval env expr in
      if value <> Expr.unit then
        print (Eval.show_value value);
    with
    | Failure msg
    | Invalid_argument msg ->
      print @@ (bright_red " * Error" ^ ": " ^ msg);
      flush stdout
    | exn ->
      let msg = Exn.to_string exn in
      print @@ (bright_red " * Error" ^ ": " ^ msg);
      flush stdout
  done

let () = begin
  print (end_color ^ "\n" ^ fold_logo ^ "\n");
  loop ();
end

