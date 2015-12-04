
open Elements
open Pratt.Foundation
open Pratt.Syntax
open Pratt.Lexer
open Pratt.Grammar
open Pratt.Env
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

let bin_numeric_function f args =
  let args_num = (List.map ~f:unpack_number args) in
  Atom (Int (if List.length args < 2
             then List.fold ~init:0 ~f args_num
             else List.reduce_exn   ~f args_num))

let unary_numeric_function f = function
  | [arg] ->
    let num = unpack_number arg in
    Atom (Int (f num))
  | _ -> fail "function expects 1 argument."

let factorial i =
  let rec loop acc i =
    if i = 0 then acc
    else loop (i * acc) (i - 1) in
  loop 1 i

module Core = Map.Make(String)
let core =
  Core.empty
  |> Core.add ~key:"+" ~data:(bin_numeric_function ( + ))
  |> Core.add ~key:"-" ~data:(bin_numeric_function ( - ))
  |> Core.add ~key:"/" ~data:(bin_numeric_function ( / ))
  |> Core.add ~key:"*" ~data:(bin_numeric_function ( * ))
  |> Core.add ~key:"%" ~data:(bin_numeric_function (fun x y -> x mod y))
  |> Core.add ~key:"!" ~data:(unary_numeric_function factorial)


let rec quasiquote expr =
  match expr with
  | List [Atom (Sym "unquote"); expr] -> expr
  | List (x :: xs) -> List [Atom (Sym "cons"); quasiquote x; quasiquote (List xs)]
  | expr -> List [Atom (Sym "quote"); expr]

let rec apply f args =
  match f with
  | Atom (Sym name) ->
    (* fail "bad apply" *)
    begin
      try (Env.empty, (Core.find name core) args)
      with Not_found ->
        raise (Invalid_argument (fmt "function %s is not defined" (Expr.show f)))
    end
  | Atom _ ->
    raise (Invalid_argument (fmt "function required, got %s" (Expr.show f)))
  | List _ ->
    fail "direct function application not implemented"
  | Func (params, body, env) ->
    Log.info "Apply a lambda function...";
    Log.info (fmt "params: [%s]" (join ", " (List.map ~f:Expr.show params)));
    Log.info (fmt "args: [%s]" (join ", " (List.map ~f:Expr.show args)));
    Log.info (fmt "body: %s" (Expr.show body));
    let nparam = List.length params in
    let nargs  = List.length args in
    if nparam <> nargs then
      fail (fmt "function expects %d arguments, got %d" nparam nargs)
    else
      let rec bind_args env pl al =
        match pl, al with
        | []     , []      -> env
        | p :: pl, a :: al ->
          let env' = Env.add (Expr.unwrap_sym p) a env in
          bind_args env' pl al
        | _ -> fail "Impossible: already enforced same length." in
      let env' = bind_args env params args in
      eval env' body

and eval env expr =
  match expr with
  (* Symbols are looked-up in the environment. *)
  | Atom (Sym name) -> (env, Env.get name env)

  (* Other atoms (Bool, Char, Float, Int, Str) self-evaluate to themselves. *)
  | Atom x
    -> (env, expr)

  (* Definitions store new values in the environment. *)
  | List [Atom (Sym "=");      Atom (Sym name); body]
  | List [Atom (Sym "define"); Atom (Sym name); body] ->
    let (env, value) = (eval env body) in
    (Env.add name value env, Expr.unit)

  | List (Atom (Sym "=" as f) :: args)
  | List (Atom (Sym "define" as f) :: args) ->
    fail (fmt "%s expects 2 arguments, got %d" (show_literal f) (List.length args))

  (* Lambda function expression. *)
  | List [Atom (Sym "->");     List args; body]
  | List [Atom (Sym "lambda"); List args; body] ->
    let subenv = Env.make (Some env) in
    (env, Func (args, body, subenv))

  | List [Atom (Sym "->");     Atom x as arg; body]
  | List [Atom (Sym "lambda"); Atom x as arg; body] ->
    let subenv = Env.make (Some env) in
    (env, Func ([arg], body, subenv))

  | List (Atom (Sym "->" as f) :: args)
  | List (Atom (Sym "lambda" as f) :: args) ->
    fail (fmt "%s expects 2 arguments, got %d" (show_literal f) (List.length args))

  (* Quoting and quasiquoting. *)
  | List [Atom (Sym "`"); expr]
  | List [Atom (Sym "quasiquote"); expr] ->
    eval env (quasiquote expr)
  | List (Atom (Sym "'") :: args)
  | List (Atom (Sym "quote") :: args) ->
    (env, List args)

  | List [Atom (Sym "if"); cond; conseq; alt] ->
    let (env, cond) = eval env cond in
    begin match cond with
      | Atom (Bool true)  -> eval env conseq
      | Atom (Bool false) -> eval env alt
      | _ -> fail (fmt "Invalid type: expected a boolean, found %s" (Expr.show cond))
    end

  (* Process the REPL directives. *)
  (* TODO: Make directives simple macros loaded by REPL on startup. *)
  (* TODO: Use the shortest matching algorithm: \h \help, etc *)
  | List (Atom (Sym "\\") :: args) ->
    begin match args with
      | [Atom (Sym "q")] -> quit ()
      | [Atom (Sym "i"); x] ->
        print (show_value x);
        (env, Expr.unit)
      | (Atom (Sym "?") :: args)
      | (Atom (Sym "i") :: args) -> fail "\\i expects one argument"
      | (Atom (Sym "h") :: args) ->
        print "Available directives: \\h, \\i, \\q.";
        (env, Expr.unit)
      | [Atom (Sym unknown)] -> fail (fmt "Unknown directive: %s" unknown)
      | _ -> fail "Unknown directive, use \\h for help"
    end

  (* Built-in functions are looked-up in the core namespace. *)
  | List (Atom (Sym "+") as f :: args) ->
    apply f (List.map args ~f:(snd % eval env))

  | List (f :: args) ->
    let env, f = eval env f in
    let args   = List.map args ~f:(snd % eval env) in
    apply f args

  | List [] ->
    (env, Expr.unit)

  | Func _ -> fail "no func"

