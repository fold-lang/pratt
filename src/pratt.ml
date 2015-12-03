
module Foundation = Pratt_foundation
module Lexer      = Pratt_lexer
module Syntax     = Pratt_syntax
module Parser     = Pratt_parser
module Grammar    = Pratt_grammar

module Log = Elements.Log

open Pratt_foundation
open Pratt_lexer
open Pratt_syntax
open Pratt_parser
open Pratt_grammar

type state = {
    lexer   : lexer;
    token   : token;
    grammar : (expr, (expr, state) parser) grammar;
    rule    : (expr, (expr, state) parser) rule;
}

let led_error : expr -> (expr, state) parser =
  fun exp -> get >>= fun { token } ->
    error @ fmt "%s cannot be used in led position." (show_token token)

let nud_error =
  get >>= fun { token } ->
    error @ fmt "%s cannot be used in nud position." (show_token token)

let (<?>) p label = fun s ->
    match p s with
      | Error _ ->
        Error (match () with
               | () when s.token.value = Sym "EOF" ->
                 (fmt "%s unexpected end of file while reading %s"
                       (show_location s.token.location) label)
               | () when label = (show_literal (Sym "EOF")) ->
                 (fmt "parsing stopped at %s" (show_token s.token))
               | () -> (fmt "expected %s but %s found"
                       label (show_token s.token)))
      | ok -> ok

let advance = get >>= fun s ->
  let token = Lexer.next s.lexer in
  let rule = lookup_rule s.grammar token in
  put { s with rule; token }

let inspect_token =
  inspect begin fun { token } ->
    print @ fmt "token: %s" (show_literal token.value)
  end

let inspect_grammar =
  inspect begin fun { grammar = g; token = t } ->
    print @ fmt "grammar(%s) = %s" (show_token t) (show_grammar g)
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

let rec parse_infix' rbp x =
  get >>= fun { rule; grammar } ->
    trace (fmt "parse_infix: sym = %s, precedence = %d, rbp = %d, left = %s"
             (show_literal rule.sym) rule.precedence rbp (Expr.show x));
    let default_led = !! ((grammar.default rule.sym).led) in
    let current_led = rule.led || default_led in
    if rule.precedence > rbp
      then current_led x >>= parse_infix' rbp
      else return x

let is_eol rule = (rule.sym = Sym "EOL")

let rec parse_infix precedence left =
  get >>= fun { rule; grammar } -> begin
    trace (fmt " infix\t%s\t%d\t%d\t%s\t%s"
             (show_literal rule.sym) rule.precedence precedence
             (if rule.precedence > precedence then "." else "!")
             (Expr.show left));

    let default = Opt.value_exn (grammar.default rule.sym).led in
    let parse = rule.led || default in
    if rule.precedence > precedence
      then parse left >>= parse_infix precedence
      else return left
  end

let parse_prefix rbp =
  get >>= fun { rule; grammar } ->
    trace (fmt "prefix\t%s\t%d\t%d\t"
           (show_literal rule.sym) rule.precedence rbp);
    let default_nud = !! ((grammar.default rule.sym).nud) in
    let current_nud = rule.nud || default_nud in
    current_nud >>= parse_infix rbp

let init ~lexer ~grammar =
  let token = read_token lexer in
  let rule = lookup_rule grammar token in
  let s = { lexer; token; grammar; rule } in
  match run (parse_prefix 0 << expect (Sym "EOF")) s with
  | Ok (value, _) -> value
  | Error msg -> raise (Failure msg)



(*
 * Rules Builders
 *)

(* let unary_prefix sym precedence = rule sym ~precedence *)
(*   ~nud: (consume sym >> *)
(*          parse_prefix >>| *)
(*          fun x -> List [Atom sym; x]) *)

let unary_postfix sym = rule sym
  ~precedence:1
  ~led:(fun x -> consume sym >> return (List [Atom sym; x]))

let binary_infix sym precedence = rule sym ~precedence
    ~led:(fun x -> consume sym >> parse_prefix precedence >>=
          fun y -> return (List [Atom sym; x; y]))

let binary_infix_right sym precedence = rule sym ~precedence
  ~led:(fun x -> consume sym >> parse_prefix (precedence - 1) >>=
        fun y -> return (List [Atom sym; x; y]))

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
        if Opt.is_some rule.led then
          parse_infix rule.precedence x
          (* parse_infix precedence x *)
        else
          parse_prefix (precedence - 1) >>= fun y ->
          return (List [Atom (Sym ";"); x; y])
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
      return @ List (match prev_expr with
        | List xs -> List.append xs [next_expr]
        | atom    -> [atom; next_expr])
    end
    ~nud:(consume sym >> return (Atom sym))


