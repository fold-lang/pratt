
module Lang = Fold_lang
module Eval = Fold_evaluator

open Pure

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
  blue ("  ▗▐▝                    \n\n") ^
  italic "  \"Simplicity is prerequisite for reliability.\"\n" ^
  "      — Edsger W. Dijkstra"


let print_value e =
  print (green " = " ^ start_white ^ (Show.expr e))


let read chan =
  create_lexer_with_channel "<REPL>" chan


let rec loop env =
  try
    print (bright_blue "->" ^ start_white) ~terminator:" " ~flush:true;
    let expr  = Pratt.parse ~lexer:(read stdin) ~grammar:Lang.core_lang in
    let (env, value) = Eval.eval env expr in
    if value <> Expr.unit ()
      then print_value value;
    loop env
  with exn ->
    print (bright_red " * Error" ^ ": " ^ (Exn.show exn)) ~flush:true;
    loop env


let is_macro_fn = function
    | [{ value = Func _; meta }] ->
      let r = Assoc.find meta (Sym "macro") = Some (Bool true) in
      Expr.bool r
    | [_] -> Expr.bool false
    | _ -> invalid_arg "function expects 2 arguments"


let is_macro = {
    value = Func is_macro_fn;
    meta = [Sym "macro",  Bool true] }


let get_meta_fn = function
    | [{ meta }] -> Expr.form (List.map (fun (k, v) -> Expr.(pair (atom k) (atom v))) meta)
    | _ -> invalid_arg "function expects 2 arguments"


let get_meta = {
    value = Func get_meta_fn;
    meta = [Sym "macro",  Bool true] }


(* TODO: Define + as a macro with 2 args: 2 + 2, and for a list: reduce? *)

let factorial i =
  let rec loop acc i =
    if i = 0 then acc
    else loop (i * acc) (i - 1) in
  loop 1 i


let env =
  Env.empty ()
  |> Env.add "T"               (Expr.atom (Bool true))
  |> Env.add "F"               (Expr.atom (Bool false))
  |> Env.add "list"            (Expr.func (fun xs -> Expr.list xs))
  |> Env.add "+"               (Eval.bin_numeric_function ( + ))
  |> Env.add "-"               (Eval.bin_numeric_function ( - ))
  |> Env.add "/"               (Eval.bin_numeric_function ( / ))
  |> Env.add "*"               (Eval.bin_numeric_function ( * ))
  |> Env.add "!"               (Eval.unary_numeric_function factorial)
  |> Env.add "cons"            Eval.cons
  |> Env.add "is_macro"        is_macro
  |> Env.add "meta"            get_meta

let env =
  env |> Env.add "eval"  (Expr.func (function [expr] -> snd (Eval.eval env expr)
                                            | _ -> invalid_arg "eval expects one argument"))

let () = begin
  print (end_color ^ "\n" ^ fold_logo ^ "\n");
  loop env
end

