
module Foundation = Pratt_foundation
module Lexer      = Pratt_lexer
module Syntax     = Pratt_syntax
module Parser     = Pratt_parser
module Grammar    = Pratt_grammar
module Env        = Pratt_env

module Log = Elements.Log

open Pratt_foundation
open Pure
open Pratt_lexer
open Pratt_syntax
open Pratt_grammar

module Parser = Pratt_parser


module State = struct
  type t =
    { lexer   : lexer;
      token   : token;
      grammar : (expr, (expr, state) parser) grammar;
      rule    : (expr, (expr, state) parser) rule }
end

module Parser = Pratt_parser.Make(State)

open Parser



let led_error : expr -> (expr, state) parser =
  fun exp -> get >>= fun { token } ->
    error ("%s cannot be used in led position." % show_token token)

let nud_error =
  get >>= fun { token } ->
    error ("%s cannot be used in nud position." % show_token token)

let (<?>) p label = fun s ->
    match p s with
      | Error _ ->
        Error (match () with
               | () when s.token.value = Sym "EOF" ->
                 "%s unexpected end of file while reading %s" %
                    (show_location s.token.location, label)
               | () when label = (show_literal (Sym "EOF")) ->
                 "parsing stopped at %s" % show_token s.token
               | () -> "expected %s but %s found" %
                       (label, show_token s.token))
      | ok -> ok

let advance = get >>= fun s ->
  let token = Lexer.next s.lexer in
  let rule = lookup_rule s.grammar token in
  put { s with rule; token }

let inspect_token =
  inspect begin fun { token } ->
    print ("token: %s" % show_literal token.value)
  end

let inspect_grammar =
  inspect begin fun { grammar = g; token = t } ->
    print ("grammar(%s) = %s" % (show_token t, show_grammar g))
  end

let expect x =
  satisfy (fun { token } -> token.value = x) <?> (show_literal x)

let consume x =
  expect x >> advance

let push_scope scope =
  get >>= fun s ->
    put { s with grammar = { s.grammar with env = scope::s.grammar.env } }

let pop_scope =
  get >>= fun s ->
    match s.grammar.env with
    | _::env -> put { s with grammar = { s.grammar with env = env } }
    | [] -> raise (Failure "cannot pop scope on empty grammar")

let with_scope s p =
  match s with
  | None -> p
  | Some s' -> push_scope s' >> p

(* TODO: Move the default rule lookup to Grammar. *)

let is_eol rule = (rule.sym = Sym "EOL")

let rec parse_infix precedence left =
  get >>= fun { rule; grammar } -> begin
    trace (" infix\t%s\t%d\t%d\t%s" % (show_literal rule.sym, rule.precedence, precedence, Show.expr left));

    let default = Option.force (grammar.default rule.sym).led in
    let parse = rule.led or default in
    if rule.precedence > precedence
      then parse left >>= parse_infix precedence
      else return left
  end

let parse_prefix rbp =
  state <- get;
  let { rule; grammar } = state in
  trace ("prefix\t%s\t%d\t%d\t" % (show_literal rule.sym, rule.precedence, rbp));
  let default_nud = Option.force (grammar.default rule.sym).nud in
  let current_nud = rule.nud or default_nud in
  left <- current_nud;
  parse_infix rbp left

let parse ~lexer ~grammar =
  let token = read_token lexer in
  let rule  = lookup_rule grammar token in
  let state = { lexer; token; grammar; rule } in
  match run (parse_prefix 0 << expect (Sym "EOF")) state with
  | Ok (value, _) -> value
  | Error msg -> raise (Failure msg)

(*
 * Rules Builders
 *)

(* let unary_prefix sym precedence = rule sym ~precedence *)
(*   ~nud: (consume sym >> *)
(*          parse_prefix >>| *)
(*          fun x -> List [Atom sym; x]) *)

(* x f => (f: x) *)
let unary_postfix sym = rule sym
  ~precedence:70
  ~led:(fun x -> consume sym >> return (Expr.call (Expr.atom sym) [x]))

let binary_infix sym precedence =
  rule sym ~precedence ~led:
    begin%monad fun x ->
      perform (consume sym);
      y <- parse_prefix precedence;
      return (Expr.call (Expr.atom sym) [x; y])
    end


let binary_infix sym precedence =
  rule sym ~precedence ~led:
    begin%monad fun x ->
      perform (consume sym);
      y <- parse_prefix precedence;
      return (Expr.call (Expr.atom sym) [x; y])
    end



let binary_infix_right sym precedence = rule sym ~precedence
  ~led:(fun x -> consume sym >> parse_prefix (precedence - 1) >>=
         fun y -> return (Expr.call (Expr.atom sym) [x; y]))

(* Try to read the next led operator, if defined, continue parsing, otherwise,
   create a seq with the previous expression x and the next prefix y.
   Newline is right associative just like ';'. *)
let end_of_line =
  let sym = Sym "EOL" in
  let precedence = 10 in
  rule sym
    ~precedence
    ~led:begin fun x ->
      consume sym >>
      get >>= fun { rule; grammar } ->
        if Option.is_some rule.led then
          parse_infix rule.precedence x
          (* parse_infix precedence x *)
        else
          parse_prefix (precedence - 1) >>= fun y ->
          return (Expr.seq x y)
    end
  ~nud:(consume sym >> parse_prefix precedence)

(* Delimiters separate expressions without affecting the parsing tree.
   The left binding power for delimiters is low since they are strong separators
   and must stop the parsing in `parse_infix`. *)
let delimiter sym = rule sym
    ~precedence:0
    ~nud:nud_error (* Delimiter should not start and expression *)
    ~led:led_error (* Illegal delimiter parsing. Delimiters must be consumed. *)

(* Groups behave like regular symbols and thus their left binding power has to be high
   (close or equal to default) to allow list parsing in `parse_infix`. *)
let group start_sym end_sym =
  let group_scope =
    Scope.(define (delimiter end_sym) empty) in
  rule start_sym
    ~precedence:80
    ~nud:begin
      consume start_sym >>
      push_scope group_scope >>
      parse_prefix 0 >>= fun expr ->
      pop_scope >>
      consume end_sym >>
      return expr
    end

(* The default rule is essential for the correct parsing of atoms and lists.
   This rule's nud will create atomic expressions with the undefined symbols.
   And the led will create lists of expressions.
   The default rule has the highest left binding power. *)
let default sym = rule sym
    ~precedence:90
    ~led:begin fun prev_expr ->
      parse_prefix 90 >>= fun next_expr ->
      (* TODO: Replace by cons. *)
      let list = match prev_expr with
        | { value = Form xs } -> Expr.list (List.append xs [next_expr])
        | _ -> Expr.list [prev_expr; next_expr] in
      return list
    end
    ~nud:(consume sym >> return (Expr.atom sym))


