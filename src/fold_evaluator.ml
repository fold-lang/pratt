

open Elements
open Pratt.Foundation
open Pratt.Syntax
open Pratt.Lexer
open Pratt.Grammar
open Fold

let quit () =
  print (yellow " ! " ^ start_white ^ "Exiting...");
  exit 0

let show_value x =
  green " = " ^ start_white ^ (Expr.show x)

let unpack_number nexpr =
  match nexpr with
  | Atom (Int x) -> x
  | x -> fail (fmt "Invalid type: expected a number, found %s" (Expr.show x))

let numeric_function f args =
  let args_num = (List.map ~f:unpack_number args) in
  Atom (Int (if List.len args < 2
             then List.fold ~init:0 ~f args_num
             else List.reduce_exn   ~f args_num))

module Core = Map.Make(String)
let core =
  Core.empty
  |> Core.add "+"  (numeric_function ( + ))
  |> Core.add "-"  (numeric_function ( - ))
  |> Core.add "/"  (numeric_function ( / ))
  |> Core.add "*"  (numeric_function ( * ))
  |> Core.add "%"  (numeric_function (fun x y -> x mod y))


let rec quasiquote expr =
  match expr with
  | List [Atom (Sym "unquote"); expr] -> expr
  | List (x :: xs) -> List [Atom (Sym "cons"); quasiquote x; quasiquote (List xs)]
  | expr -> List [Atom (Sym "quote"); expr]

let apply f args =
  try
    (Core.find f core) args
  with Not_found -> raise (Invalid_argument ("function `" ^ f ^ " is not defined"))

let rec eval env expr =
  match expr with
  (* Symbols are looked-up in the environment. *)
  | Atom (Sym s) -> Env.get env expr

  (* Other atoms (Bool, Char, Float, Int, Str) self-evaluate to themselves. *)
  | Atom x
    -> expr

  (* Definitions store new values in the environment. *)
  | List [Atom (Sym "="); key; body]
  | List [Atom (Sym "define"); key; body] ->
    let value = (eval env body) in
    Env.set env key value;
    Expr.unit

  | List (Atom (Sym "=" as f) :: args)
  | List (Atom (Sym "define" as f) :: args) ->
    fail (fmt "%s expects 2 arguments, got %d" (show_literal f) (List.len args))

  (* Quoting and quasiquoting. *)
  | List [Atom (Sym "`"); expr]
  | List [Atom (Sym "quasiquote"); expr] ->
    eval env (quasiquote expr)
  | List (Atom (Sym "'") :: args)
  | List (Atom (Sym "quote") :: args) ->
    List args

  | List [Atom (Sym "if"); cond; conseq; alt] ->
    let r = eval env cond in
    begin match r with
      | Atom (Bool true)  -> eval env conseq
      | Atom (Bool false) -> eval env alt
      | _ -> fail (fmt "Invalid type: expected a boolean, found %s" (Expr.show r))
    end

  (* Process the REPL directives. *)
  | List (Atom (Sym "\\") :: args) ->
    begin match args with
      | [Atom (Sym "q")] -> quit ()
      | [Atom (Sym "i"); x] -> print (show_value x); Expr.unit
      | (Atom (Sym "?") :: args)
      | (Atom (Sym "h") :: args) -> print "Available directives: \\h, \\i, \\q."; Expr.unit
      | (Atom (Sym "i") :: args) -> fail "\\i expects one argument"
      | [Atom (Sym unknown)] -> fail (fmt "Unknown directive: %s" unknown)
      | _ -> fail "Unknown directive, use \\h for help"
    end

  (* Built-in functions are looked-up in the core namespace. *)
  | List (Atom (Sym f) :: args) ->
    apply f (List.map args ~f:(eval env))

  | List [] ->
    Expr.unit

  | expr -> expr

