
open Foundation
open Lexer
open Syntax
open Parser
open Grammar

type state = {
    lexer   : lexer;
    token   : token;
    grammar : (expr, (expr, state) parser) grammar;
    rule    : (expr, (expr, state) parser) rule;
}

let led_error : (expr, state) parser =
  get >>= fun { token } ->
    error @ "%s cannot be used in led position." % (show_token token)

let nud_error =
  get >>= fun { token } ->
    error @ "%s cannot be used in nud position." % (show_token token)

let (<?>) p label = fun s ->
    match p s with
      | Error _ ->
        Error (match () with
               | () when s.token.value = Sym "EOF" ->
                 (~% "%s unexpected end of file while reading %s"
                       (show_location s.token.location) label)
               | () when label = (show_literal (Sym "EOF")) ->
                 (~% "parsing stopped at %s" (show_token s.token))
               | () -> (~% "expected %s but %s found"
                       label (show_token s.token)))
      | ok -> ok

let advance = get >>= fun s ->
  let token = read_token s.lexer in
  let rule = lookup_rule s.grammar token in
  put { s with rule; token }

let inspect_token =
  inspect (fun { token } ->
    print @ "token: %s" % (show_literal token.value))

let inspect_grammar =
  inspect (fun { grammar = g; token = t } ->
    print @ "grammar(%s) = %s" %% (show_token t, show_grammar g))

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

(* Parses a nud sequence. Requires the first symbol to have only nud. *)
let parse_nud =
  get >>= fun { rule; grammar } ->
    trace (~% "parse_nud: rule.sym = %s" (show_literal rule.sym));
    match rule.led with
    | None ->
      begin match rule.nud with
        | Some nud -> nud
        | None -> Option.value (grammar.default rule.sym).nud ~default:nud_error
      end
    | Some led -> nud_error

let rec parse_led rbp x =
  get >>= fun { rule; grammar } ->
    trace (~% "parse_led: sym = %s, left = %s, lbp = %d, rbp = %d"
              (show_literal rule.sym) (show_expr x) rule.lbp rbp);
    if rule.lbp > rbp then
      let led = match rule.led with
        | Some led -> led
        | None -> fun x -> led_error in
        (* | None -> Option.value (grammar.default rule.sym).led *)
        (*             ~default:(fun x -> led_error) in *)
      led x >>= parse_led rbp
    else return x

let parse_expr rbp =
  get >>= fun { rule; grammar } ->
    trace (~% "parse_exp: rule.sym = %s" (show_literal rule.sym));
    let nud = match rule.nud with
      | Some nud -> nud
      | None -> Option.value (grammar.default rule.sym).nud ~default:nud_error in
    nud >>= parse_led rbp

let init ~lexer ~grammar () =
  let token = read_token lexer in
  let rule = lookup_rule grammar token in
  let s = { lexer; token; grammar; rule } in
  match run (parse_expr 0 << expect (Sym "EOF")) s with
  | Ok (value, _) -> value
  | Error msg -> raise (Failure msg)


