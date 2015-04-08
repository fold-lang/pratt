
open Foundation
open Lexer
open Syntax
open Parser

type state = { lexer   : lexer;
               grammar : (token -> handler) Grammar.t;
               handler : handler }

and handler = { token : token;
                lbp   : int;
                nud   : (expr, state) parser option;
                led   : (expr -> (expr, state) parser) option;
                scope : (token -> handler) Grammar.Scope.t option }

(* -- Error Handling -- *)

let led_error =
  get >>= fun { handler } ->
    error $ "%s cannot be used in infix position." % (show_token handler.token)

let nud_error =
  get >>= fun { handler } ->
    error $ "%s cannot be used in prefix position." % (show_token handler.token)

let (<?>) p label = fun s ->
    match p s with
    | Error _ -> Error ("%s found but %s expected" %%
                  (show_token s.handler.token, label))
    | ok -> ok

let advance = get >>= fun s ->
  let token = read_token s.lexer in
  let name = string_of_literal token.value in
  let binding = Grammar.lookup s.grammar name in
  void $ ~% "advance: token = %s, scope = %s" (show_token token) (Grammar.show s.grammar);
  let handler = binding token in
  put { s with handler }

let inspect_token =
  inspect (fun {handler} ->
    print $ "token: %s" % (show_literal handler.token.value))

let inspect_scope =
  inspect (fun {grammar; handler} ->
    match grammar with
    | (scope::env, default) ->
      print $ "scope(%s): %s" %% (show_literal handler.token.value, Grammar.show_scope scope)
    | ([], _) -> raise (Failure "empty grammar"))

let inspect_grammar =
  inspect (fun {grammar; handler} ->
    print $ "grammar(%s) = %s" %% (show_token handler.token, Grammar.show grammar))

let expect x =
  satisfy (fun s -> s.handler.token.value = x) <?> (show_literal x)

let consume x =
  expect x >> advance

let push_scope scope =
  get >>= fun s ->
    let (env, default) = s.grammar in
    put { s with grammar = (scope::env, default) }

let pop_scope =
  get >>= fun s ->
    match s.grammar with
    | (_::env, default) -> put { s with grammar = (env, default) }
    | ([], _) -> raise (Failure "cannot pop scope on empty grammar")

let with_scope s p =
  match s with
  | None -> p
  | Some s' -> push_scope s' >> p

let rec parse_next rbp x =
  get >>= fun { handler } ->
    void (~% "parse_next: token = %s, x = %s, lbp = %d, rbp = %d"
    (show_literal handler.token.value) (show_expr x) handler.lbp rbp);
  if handler.lbp > rbp then
      match handler.led with
      | Some led -> advance >> led x >>= parse_next rbp
      | None -> led_error
    else return x

let parse_nud rbp =
  get >>= fun { handler } ->
  match handler.led with
  | None -> (match handler.nud with
    | Some nud -> advance >> nud
    | None -> nud_error)
  | Some _ -> nud_error

let parse_expr rbp =
  get >>= fun { handler } ->
    void ("parse_expr: token = %s, rbp = %d" %% (show_literal handler.token.value, rbp));
    match handler.nud with
    | Some nud -> advance >> nud >>= parse_next rbp
    | None -> nud_error

let init ~lexer ~grammar () =
  let token = read_token lexer in
  let name = string_of_literal token.value in
  let state = { lexer;
                grammar;
                handler = (Grammar.lookup grammar name) token } in
  match run (parse_expr 0 << expect (Symbol "EOF")) state with
  | Ok (value, _) -> value
  | Error msg -> raise (Failure msg)


