
open Elements
open Pratt.Foundation
open Pratt.Lexer
open Pratt.Syntax
open Pratt.Grammar
open Pratt.Env
open Fold_lang

(* module Log = struct let info x = () end *)

let show_env x = Show.assoc id Show.expr (Scope.to_assoc x.data)

let quit () =
  print (yellow " ! " ^ start_white ^ "Exiting...");
  exit 0

let show_value x =
  green " = " ^ start_white ^ (Show.expr x)

let bin_numeric_function f = Expr.func @ fun args ->
  let args_num = (List.map ~f:Expr.unwrap_int args) in
  Expr.int (if List.length args < 2
            then List.fold ~init:0 ~f args_num
            else List.reduce_exn   ~f args_num)

let unary_numeric_function f = Expr.func (function
  | [arg] ->
    let num = Expr.unwrap_int arg in
    Expr.int (f num)
  | _ -> fail "function expects 1 argument.")

let cons = Expr.func @ fun args ->
  match args with
  | [x; { value = Form xs }] -> Expr.list (x :: xs)
  | [_; _] -> invalid_arg "second argument must be a term"
  | _ -> invalid_arg "function expects 2 arguments"

let rec quasiquote expr =
  match expr with
  | { value = Form [{ value = Atom (Sym "unquote") }; expr] } -> expr
  | { value = Form (x :: xs) } ->
    Expr.form [Expr.sym "cons"; quasiquote x; quasiquote (Expr.form xs)]
  | expr -> Expr.form [Expr.sym "quote"; expr]

let is_macro_call env expr =
  let r = match expr with
  | { value = Form ({ value = Atom (Sym name) } :: args) } ->
    begin
      match guard (Env.get name) env with
      | Some { value = Func macro; meta } ->
        Dict.find meta (Sym "macro") = Some (Bool true)
      | Some _ | None -> false
    end
  | _ -> false in
  Log.info @ fmt "is_macro_call: %b" r;
  r

let rec macroexpand env expr =
  Log.info @ fmt "macroexpand: expr = %s" (Show.expr expr);
  if is_macro_call env expr
  then match expr with
    | { value = Form ({ value = Atom (Sym name) } :: args) } ->
      begin match guard (Env.get name) env with
        | Some { value = Func f } ->
          macroexpand env (f args)
        | _ -> expr
      end
    | _ -> expr
  else expr

let rec eval_expr env expr =
  Log.info @ fmt "eval_expr: expr = %s" @ Show.expr expr;
  match expr with
  | { value = Atom (Sym x) } -> env, Env.get x env
  | { value = Form xs } ->
    let value = Form (List.map ~f:(fun x -> snd (eval env x)) xs) in
    env, { expr with value }
  | _ -> env, expr

and eval env expr =
  let expanded_expr = macroexpand env expr in
  match expanded_expr with

  (*
   * Special Forms
   *)

  | { value = Form [{ value = Atom (Sym "=") }     ; { value = Atom (Sym name) }; body] }
  | { value = Form [{ value = Atom (Sym "define") }; { value = Atom (Sym name) }; body] } ->
    let (env, value) = (eval env body) in
    (Env.add name value env, Expr.unit ())

  | { value = Form [{ value = Atom (Sym "macro") }; { value = Atom (Sym name) }; body] } ->
    Log.info @ fmt "eval: macro definition: %s" name;
    let env', body' = eval env body in
    let macro =
      match body' with
      | { value = Func f; meta = meta } ->
        { value = Func f; meta = Dict.add meta (Sym "macro") (Bool true) }
      | _ -> invalid_arg "macro value must be a function" in
    (Env.add name macro env, Expr.unit ())

  | { value = Form [{ value = Atom (Sym "let") }; { value = Form bindings }; body] } ->
    let local_env = Env.make (Some env) in
     let rec bind_pairs env bindings =
       begin
         match bindings with
         | { value = Atom (Sym name) } :: exp :: more ->
           let env', exp' = eval env exp in
           bind_pairs (Env.add name exp' env') more
         | e :: _ :: _ ->
           invalid_arg (fmt "invalid binding expression %s" (Show.expr e))
         | _ :: [] ->
           invalid_arg "let requires an even number of bindings"
         | [] -> env
       end in
    let env' = bind_pairs local_env bindings in
    eval env' body

  | { value = Form ({ value = Atom (Sym "do") } :: args) } ->
    List.fold_left args ~init:(env, Expr.unit ())
      ~f:(fun (env, r) exp -> eval env exp)

  | { value = Form [{ value = Atom (Sym "if") }; c; t; f] } ->
    let (env, c') = eval env c in
    if Expr.unwrap_bool c'
      then eval env t
      else eval env f

  | { value = Form [{ value = Atom (Sym "->") }; { value = Atom _ } as arg; body] } ->
    eval env (Expr.list [Expr.sym "->"; Expr.list [arg]; body])

  | { value = Form [{ value = Atom (Sym "->") }; { value = Form names }; body] } ->
    let f args =
      let local_env = Env.make (Some env) in
      let rec bind_args env names args =
        begin match names, args with
          | [{ value = Atom (Sym "&") }; { value = Atom (Sym name) }], args ->
            Env.add name (Expr.list args) env
          | ({ value = Atom (Sym name) } :: names), (arg :: args) ->
            bind_args (Env.add name arg env) names args;
          | [], [] -> env
          | _ -> invalid_arg "bad param count in fn call"
        end in
      let env' = bind_args local_env names args in
      let _, retval = eval env' body in
      retval in
    (env, Expr.func f)

  | { value = Form [{ value = Atom (Sym "quote") }; quoted_expr] } ->
    env, quoted_expr

  | { value = Form [{ value = Atom (Sym "quasiquote") }; quasiquoted_expr] } ->
    eval env (quasiquote quasiquoted_expr)

  | { value = Form [{ value = Atom (Sym "macroexpand") }; macro] } ->
    env, macroexpand env macro

  | { value = Form [{ value = Atom (Sym "is_macro_call") }; call] } ->
    env, Expr.bool (is_macro_call env call)

  (* Process the REPL directives. *)
  (* TODO: Make directives simple macros loaded by REPL on startup. *)
  (* TODO: Use the shortest matching algorithm: \h \help, etc *)
  | { value = Form ({ value = Atom (Sym "\\") } :: args) } ->
    begin match args with
      | [{ value = Atom (Sym "q") }] -> quit ()
      | [{ value = Atom (Sym "env") }] -> Log.info @ show_env env; (env, Expr.unit ())
      | [{ value = Atom (Sym "i") }; x] ->
        print (show_value x);
        (env, Expr.unit ())
      | { value = Atom (Sym "?") } :: args
      | { value = Atom (Sym "i") } :: args -> fail "\\i expects one argument"
      | { value = Atom (Sym "h") } :: args ->
        print "Available directives: \\h, \\i, \\q.";
        (env, Expr.unit ())
      | [{ value = Atom (Sym unknown) }] -> fail (fmt "Unknown directive: %s" unknown)
      | _ -> fail "Unknown directive, use \\h for help"
    end

  (*
   * Application Form
   *)

  | { value = Form _ } ->
    begin match eval_expr env expanded_expr with
      | env', { value = Form ({ value = Func f } :: args) } ->
        Log.info @ fmt "eval: function application";
        env', f args
      | _, exp ->
        invalid_arg @ fmt "%s is not a function, cannot be applied" (Show.expr exp)
    end

  | _ -> eval_expr env expanded_expr



  (* | Form [Atom (Sym "eval"); code] -> *)
    (* let env', exp = (eval env code) in *)
    (* eval env' exp *)

  (* | Form (Atom (Sym "eval" as f) :: args) -> *)
    (* fail (fmt "%s expects 2 arguments, got %d" (show_literal f) (List.length args)) *)

  (* | Form (func_name :: args) -> *)
    (* let env, func_expr = eval env func_name in *)
    (* let args = List.map args ~f:(snd % eval env) in *)
    (* apply func_expr args *)

  (* | Form [] -> *)
    (* (env, Exp.unit) *)

  (* | Lambda _ -> fail "no lambda" *)

