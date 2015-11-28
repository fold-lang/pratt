
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

let led_error : expr -> (expr, state) parser =
  fun expr -> get >>= fun { token } ->
    error @ fmt "%s cannot be used in led position." (show_token token)

let nud_error =
  get >>= fun { token } ->
    error @ fmt "%s cannot be used in nud position." (show_token token)

let (<?>) p label = fun s ->
    match p s with
      | Error _ ->
        Error (match () with
               | () when s.token.value = Sym "EOF" ->
                 (fmt "%s unexprected end of file while reading %s"
                       (show_location s.token.location) label)
               | () when label = (show_literal (Sym "EOF")) ->
                 (fmt "parsing stopped at %s" (show_token s.token))
               | () -> (fmt "exprected %s but %s found"
                       label (show_token s.token)))
      | ok -> ok

let advance = get >>= fun s ->
  let token = read_token s.lexer in
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

let exprect x =
  satisfy (fun { token } -> token.value = x) <?> (show_literal x)

let consume x =
  exprect x >> advance

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

let rec parse_led rbp x =
  get >>= fun { rule; grammar } ->
    trace (fmt "parse_led: sym = %s, left = %s, lbp = %d, rbp = %d"
             (show_literal rule.sym) (show_expr x) rule.lbp rbp);
    let default_led = !! ((grammar.default rule.sym).led) in
    let current_led = rule.led || default_led in
    if rule.lbp > rbp
      then current_led x >>= parse_led rbp
      else return x

let parse_one =
  get >>= fun { rule; grammar } ->
    trace (fmt "parse_one: rule.sym = %s" (show_literal rule.sym));
    let default_nud = !! ((grammar.default rule.sym).nud) in
    let current_nud = rule.nud || default_nud in
    current_nud >>= return

let parse_nud rbp =
  get >>= fun { rule; grammar } ->
    trace (fmt "parse_nud: rule.sym = %s" (show_literal rule.sym));
    let default_nud = !! ((grammar.default rule.sym).nud) in
    let current_nud = rule.nud || default_nud in
    current_nud >>= parse_led rbp


let init ~lexer ~grammar =
  let token = read_token lexer in
  let rule = lookup_rule grammar token in
  let s = { lexer; token; grammar; rule } in
  match run (parse_nud 0 << exprect (Sym "EOF")) s with
  | Ok (value, _) -> value
  | Error msg -> raise (Failure msg)



(*
 * Rules Builders
 *)

(* let unary_prefix sym lbp = rule sym ~lbp *)
(*   ~nud: (consume sym >> *)
(*          parse_nud >>| *)
(*          fun x -> Term [Atom sym; x]) *)

let atomic sym = rule sym
    ~nud:(consume sym >> return (atom sym))

let unary_postfix sym =
  fun x -> return (Term (Atom sym, [x]))

let binary_infix sym lbp = rule sym ~lbp
    ~led:(fun x -> consume sym >> parse_nud lbp >>=
          fun y -> return (Term (Atom sym, [x; y])))

let binary_infix_right sym lbp = rule sym ~lbp
  ~led:(fun x -> consume sym >> parse_nud (lbp - 1) >>=
       fun y -> return (Term (Atom sym, [x; y])))

(* Try to read the next led operator, if defined, continue parsing, otherwise,
   create a seq with the previous exprression x and the next prefix y.
   Newline is right associative just like ';'. *)
let newline sym = rule sym
    ~lbp:(10 - 1)
    ~led:begin fun x ->
      consume sym >>
      get >>= fun { rule; grammar } ->
        if Opt.is_some rule.led then
          parse_led (10 - 1) x
        else
          parse_nud (10 - 2) >>= fun y ->
          return (seq x y)
    end
  ~nud:(consume sym >> parse_nud (10 - 1))

(* Delimiters separate exprressions without affecting the parsing tree.
   The left binding power for delimiters is low since they are strong separators
   and must stop the parsing in `parse_led`. *)
let delimiter sym = rule sym
    ~lbp:0
    ~nud:nud_error (* Delimiter should not start and exprression *)
    ~led:led_error (* Illegal delimiter parsing. Delimiters must be consumed. *)

(* Groups behave like regular symbols and thus their left binding power has to be high
   (close or equal to default) to allow list parsing in `parse_led`. *)
let group start_sym end_sym =
  let group_scope =
    Scope.(define (delimiter end_sym) empty) in
  rule start_sym
    ~lbp:80
    ~nud:begin
      consume start_sym >>
      push_scope group_scope >>
      parse_nud 0 >>= fun expr ->
      pop_scope >>
      consume end_sym >>
      return expr
    end

(* The default rule is essential for the correct parsing of atoms and lists.
   This rule's nud will create atomic exprressions with the undefined symbols.
   And the led will create lists of exprressions.
   The default rule has the highest left binding power. *)
let default sym = rule sym
    ~lbp:90
    ~led:begin fun prev_expr ->
      parse_nud 90 >>= fun next_expr ->
      return (match prev_expr with
          | Term (f, xs) -> Term (f, List.append xs [next_expr])
          | atom    -> Term (atom, [next_expr]))
    end
    ~nud:(consume sym >> return (Atom sym))


