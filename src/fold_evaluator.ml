
open Elements
open Pratt.Foundation
open Pratt.Syntax
open Pratt.Lexer
open Pratt.Grammar
open Pratt.Env
open Fold

let show_env x = Show.assoc id Show.exp (Scope.to_assoc x.data)

let quit () =
  print (yellow " ! " ^ start_white ^ "Exiting...");
  exit 0

let show_value x =
  green " = " ^ start_white ^ (Show.exp x)

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

let pair x y  = Expr.call (Expr.sym ",") [x; y]

let table = Expr.func @ fun items ->
  let rec loop acc items =
    match items with
    | k :: v :: rest ->
      loop (pair k v :: acc) rest
    | [] -> acc
    | _ -> invalid_arg "table requires an even number of items"
  in
    Expr.list (loop [] items)

let table_add t k v =
  match t with
  | List expr -> List { expr with data = pair k v :: expr.data }
  | _ -> invalid_arg "table_add expects a table"

let table_get t k =
  match t with
  | List { data = xs } ->
    (* FIXME: Will fail if k has metadata, consider alt expr type. *)
    List.fold_left xs ~init:None ~f:(fun r a -> if a = k then Some a else r)
    |> Option.value_exn
  | _ -> invalid_arg "table_add expects a table"

let cons = Expr.func @ fun args ->
  match args with
  | [x; List { data = xs }] -> Expr.list (x :: xs)
  | [_; _] -> invalid_arg "second argument must be a term"
  | _ -> invalid_arg "function expects 2 arguments"


let rec quasiquote expr =
  match expr with
  | List { data = [Atom { data = (Sym "unquote") }; expr] } -> expr
  | List { data = (x :: xs) } ->
    Expr.list [Expr.sym "cons"; quasiquote x; quasiquote (Expr.list xs)]
  | expr -> Expr.list [Expr.sym "quote"; expr]

let rec macroexpand env ast =
  match ast with
  | List { data = (Atom { data = (Sym name) } :: args) } ->
    begin
      match guard (Env.get name) env with
      | Some (Func { data = f; meta = Some t})
        when Expr.unwrap_bool (table_get t (Expr.sym "is_macro"))
        -> macroexpand env (f args)
      | _ -> ast
    end
  | _ -> ast

let rec eval_ast env ast =
  Log.info @ fmt "eval_ast: ast = %s" @ Show.exp ast;
  match ast with
  | Atom { data = (Sym x) } -> env, Env.get x env
  | List { data = xs } -> env, Expr.list (List.map ~f:(snd % eval env) xs)
  | _ -> env, ast

and eval env ast =
  Log.info @ fmt "eval: ast = %s" @ Show.exp ast;
  match macroexpand env ast with
  | List { data = [Atom { data = Sym "="      }; Atom { data = Sym name }; body] }
  | List { data = [Atom { data = Sym "define" }; Atom { data = Sym name }; body] } ->
    let (env, value) = (eval env body) in
    (Env.add name value env, Expr.unit)
  | List { data = Atom { data = Sym "="      as f } :: args }
  | List { data = Atom { data = Sym "define" as f } :: args } ->
    fail (fmt "%s expects 2 arguments, got %d" (show_literal f) (List.length args))

  | List { data = [Atom { data = Sym "macro" }; Atom { data = Sym name }; body] } ->
    begin
      let env', body' = eval env body in
      match body' with
      | Func x ->
        let old_meta = Option.(x.meta || Expr.list []) in
        let new_meta = table_add old_meta (Expr.sym "is_macro") (Expr.bool true) in
        let new_expr = Func { x with meta = Some new_meta } in
        (Env.add name new_expr env', Expr.unit)
      | _ -> invalid_arg "macro value must be a function"
    end

  | List { data = [Atom { data = Sym "let" }; List { data = bindings }; body] } ->
    let local_env = Env.make (Some env) in
     let rec bind_pairs env bindings =
       begin
         match bindings with
         | Atom { data = Sym name } :: exp :: more ->
           let env', exp' = eval env exp in
           bind_pairs (Env.add name exp' env') more
         | e :: _ :: _ ->
           invalid_arg (fmt "invalid binding expression %s" (Show.exp e))
         | _ :: [] ->
           invalid_arg "let requires an even number of bindings"
         | [] -> env
       end in
    let env' = bind_pairs local_env bindings in
    eval env' body

  | List { data = Atom { data = Sym "do" } :: args } ->
    List.fold_left args ~init:(env, Expr.unit)
      ~f:(fun (env, r) exp -> eval env exp)

  | List { data = [Atom { data = Sym "if" }; cond; then_exp; else_exp] } ->
    let (env, cond) = eval env cond in
    if Expr.unwrap_bool cond then eval env then_exp
    else eval env else_exp

  | List { data = [Atom { data = Sym "lambda" }; Atom arg; body] }
  | List { data = [Atom { data = Sym "->"     }; Atom arg; body] } ->
    eval env (Expr.list [Expr.sym "->"; Expr.list [Atom arg]; body])

  | List { data = [Atom { data = Sym "lambda" }; List { data = names }; body] }
  | List { data = [Atom { data = Sym "->"     }; List { data = names }; body] } ->
    let f args =
      Log.info @ fmt "fn: args = %s" @ Show.(list exp args);
      let local_env = Env.make (Some env) in
      let rec bind_args env names args =
        Log.info (fmt "bind_args: names = %s, args = %s"
                    Show.(list exp names) Show.(list exp args));
        begin match names, args with
          | [Atom { data = Sym "&" }; Atom { data = Sym name }], args ->
            Env.add name (Expr.list args) env
          | (Atom { data = Sym name } :: names), (arg :: args) ->
            bind_args (Env.add name arg env) names args;
          | [], [] -> env
          | _ -> invalid_arg "bad param count in fn call"
        end in
      let env' = bind_args local_env names args in
      let _, retval = eval env' body in
      Log.info @ fmt "fn: env' = %s, body = %s, retval = %s"
        (show_env env') (Show.exp body) (Show.exp retval);
      retval in
    (env, Expr.func f)

  | List { data = [Atom { data = Sym "quote"       }; ast] } -> env, ast
  | List { data = [Atom { data = Sym "quasiquote"  }; ast] } -> eval env (quasiquote ast)
  | List { data = [Atom { data = Sym "macroexpand" }; ast] } -> env, macroexpand env ast
  | List _ ->
    Log.info "function application";
    begin match eval_ast env ast with
      | env', List { data = Func { data = f } :: args } -> env', f args
      | _, exp -> invalid_arg @ fmt "%s is not a function, cannot be applied" (Show.exp exp)
    end
  | _ -> eval_ast env ast


(* let rec apply f args = *)
  (* match f with *)
  (* | Atom (Sym name) -> *)
    (* Log.info "apply: f is a symbol"; *)
    (* begin *)
      (* try (Env.empty, (Core.find name core) args) *)
      (* with Not_found -> *)
        (* raise (Invalid_argument (fmt "function %s is not defined" (Show.exp f))) *)
    (* end *)

  (* | Atom _ -> *)
    (* raise (Invalid_argument (fmt "function required, got %s" (Show.exp f))) *)

  (* | List _ -> *)
    (* fail "direct function application not implemented" *)

  (* | Lambda (params, env, body) -> *)
    (* let pnames = List.map ~f:Exp.unwrap_sym params in *)
    (* let argenv = Env.add_many env (List.combine pnames args) in *)
    (* eval argenv body *)

(* let eval_orig env ast = *)
  (* match ast with *)
  (* (* Symbols are looked-up in the environment. *) *)
  (* | Atom (Sym name) -> *)
    (* print @ fmt "resolving symbol %s" name; *)
    (* (env, Env.get name env) *)

  (* (* Other atoms (Bool, Char, Float, Int, Str) self-evaluate to themselves. *) *)
  (* | Atom x *)
    (* -> (env, expr) *)

  (* (* Definitions store new values in the environment. *) *)
  (* | List [Atom (Sym "=");      Atom (Sym name); body] *)
  (* | List [Atom (Sym "define"); Atom (Sym name); body] -> *)
    (* let (env, value) = (eval env body) in *)
    (* (Env.add name value env, Exp.unit) *)

  (* | List (Atom (Sym "=" as f) :: args) *)
  (* | List (Atom (Sym "define" as f) :: args) -> *)
    (* fail (fmt "%s expects 2 arguments, got %d" (show_literal f) (List.length args)) *)

  (*
   * Lambda function expression.
   *)

  (* (* arg0 arg1 -> body *) *)
  (* | List [Atom (Sym "->");     List args; body] *)
  (* | List [Atom (Sym "lambda"); List args; body] -> *)
    (* let closure_env = Env.make (Some env) in *)
    (* (env, Lambda (args, closure_env, body)) *)

  (* (* arg0 -> body *) *)
  (* | List [Atom (Sym "->");     Atom x as arg; body] *)
  (* | List [Atom (Sym "lambda"); Atom x as arg; body] -> *)
    (* let subenv = Env.make (Some env) in *)
    (* (env, Lambda ([arg], subenv, body)) *)

  (* (* -> e0 e1 e2 ... *) *)
  (* | List (Atom (Sym "->" as f) :: args) *)
  (* | List (Atom (Sym "lambda" as f) :: args) -> *)
    (* fail (fmt "%s expects 2 arguments, got %d" (show_literal f) (List.length args)) *)

  (*
   * Quoting and quasiquoting.
   *)

  (* (* quasiquote e0 *) *)
  (* | List [Atom (Sym "`"); expr] *)
  (* | List [Atom (Sym "quasiquote"); expr] -> *)
    (* let x = (quasiquote expr) in *)
    (* print @ fmt "quasiquoted: %s" @ Show.exp x; *)
    (* eval env x *)

  (* (* quasiquote e0 e1 *) *)
  (* | List (Atom (Sym "`" as f) :: args) *)
  (* | List (Atom (Sym "quasiquote" as f) :: args) -> *)
    (* fail (fmt "%s expects 1 argument, got %d" (show_literal f) (List.length args)) *)

  (* | List [Atom (Sym "'"); exp] *)
  (* | List [Atom (Sym "quote"); exp] -> *)
    (* (env, exp) *)

  (* | List (Atom (Sym "'" as f) :: args) *)
  (* | List (Atom (Sym "quote" as f) :: args) -> *)
    (* fail (fmt "%s expects 1 argument, got %d" (show_literal f) (List.length args)) *)

  (* | List [Atom (Sym "if"); cond; conseq; alt] -> *)
    (* let (env, cond) = eval env cond in *)
    (* begin match cond with *)
      (* | Atom (Bool true)  -> eval env conseq *)
      (* | Atom (Bool false) -> eval env alt *)
      (* | _ -> fail (fmt "Invalid type: expected a boolean, found %s" (Show.exp cond)) *)
    (* end *)

  (* (* Process the REPL directives. *) *)
  (* (* TODO: Make directives simple macros loaded by REPL on startup. *) *)
  (* (* TODO: Use the shortest matching algorithm: \h \help, etc *) *)
  (* | List (Atom (Sym "\\") :: args) -> *)
    (* begin match args with *)
      (* | [Atom (Sym "q")] -> quit () *)
      (* | [Atom (Sym "scope")] -> *)
        (* (* print (show_env env.data); *) *)
        (* print "ENV: TODO"; *)
        (* env, Exp.unit *)
      (* | [Atom (Sym "i"); x] -> *)
        (* print (show_value x); *)
        (* (env, Exp.unit) *)
      (* | (Atom (Sym "?") :: args) *)
      (* | (Atom (Sym "i") :: args) -> fail "\\i expects one argument" *)
      (* | (Atom (Sym "h") :: args) -> *)
        (* print "Available directives: \\h, \\i, \\q."; *)
        (* (env, Exp.unit) *)
      (* | [Atom (Sym unknown)] -> fail (fmt "Unknown directive: %s" unknown) *)
      (* | _ -> fail "Unknown directive, use \\h for help" *)
    (* end *)

  (* (* Built-in functions are looked-up in the core namespace. *) *)
  (* | List (Atom (Sym "+") as f :: args) -> *)
    (* apply f (List.map args ~f:(snd % eval env)) *)

  (* | List (Atom (Sym "cons") as f :: args) -> *)
    (* apply f (List.map args ~f:(snd % eval env)) *)

  (* | List [Atom (Sym "eval"); code] -> *)
    (* let env', exp = (eval env code) in *)
    (* eval env' exp *)

  (* | List (Atom (Sym "eval" as f) :: args) -> *)
    (* fail (fmt "%s expects 2 arguments, got %d" (show_literal f) (List.length args)) *)

  (* | List (func_name :: args) -> *)
    (* let env, func_expr = eval env func_name in *)
    (* let args = List.map args ~f:(snd % eval env) in *)
    (* apply func_expr args *)

  (* | List [] -> *)
    (* (env, Exp.unit) *)

  (* | Lambda _ -> fail "no lambda" *)

