
open Foundation
open Lexer
open Syntax
open Parser
open Grammar

type state = {
    lexer   : lexer;
    token   : token;
    grammar : (exp, (exp, state) parser) grammar;
    rule    : (exp, (exp, state) parser) rule;
}

let led_error : exp -> (exp, state) parser =
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
(* let parse_nud = *)
(*   get >>= fun { rule; grammar } -> *)
(*     trace (fmt "parse_nud: rule.sym = %s" (show_literal rule.sym)); *)
(*     if Opt.is_none rule.led *)
(*     then rule.nud || Opt.value_exn (grammar.default rule.sym).nud *)
(*     else nud_error *)

let rec parse_led_orig rbp x =
  get >>= fun { rule; grammar } ->
    trace (fmt "parse_led: sym = %s, left = %s, lbp = %d, rbp = %d"
              (show_literal rule.sym) (show_exp x) rule.lbp rbp);
    if rule.lbp > rbp then
      let led = rule.led || Opt.value_exn (grammar.default rule.sym).led in
      led x >>= fun exp -> parse_led_orig rbp exp
    else
      return x

(* let rec parse_led rbp x = *)
(*   get >>= fun { rule; grammar } -> *)
(*     trace (fmt "parse_led: sym = %s, left = %s, lbp = %d, rbp = %d" *)
(*               (show_literal rule.sym) (show_exp x) rule.lbp rbp); *)
(*     match rule.led with *)
(*     | None -> *)
(*       if rule.lbp > rbp then *)
(*         let led = !! ((grammar.default rule.sym).led) in *)
(*         led x >>= parse_led rbp *)
(*       else return x *)
(*     | Some led -> *)
(*       if rule.lbp > rbp then *)
(*         led x >>= parse_led rbp *)
(*       else *)
(*         return x *)

let rec parse_led rbp x =
  get >>= fun { rule; grammar } ->
    trace (fmt "parse_led: sym = %s, left = %s, lbp = %d, rbp = %d"
              (show_literal rule.sym) (show_exp x) rule.lbp rbp);
    match rule.led with
    | None ->
      if rule.lbp > rbp then
        let led = !! ((grammar.default rule.sym).led) in
        led x >>= parse_led rbp
      else
        return x
    | Some led ->
      if rule.lbp > rbp then
        led x >>= parse_led rbp
      else
        return x

let parse_exp rbp =
  get >>= fun { rule; grammar } ->
    trace (fmt "parse_exp: rule.sym = %s" (show_literal rule.sym));
    let default_nud = !! ((grammar.default rule.sym).nud) in
    let current_nud = rule.nud || default_nud in
    current_nud >>= parse_led rbp

let init ~lexer ~grammar () =
  let token = read_token lexer in
  let rule = lookup_rule grammar token in
  let s = { lexer; token; grammar; rule } in
  match run (parse_exp 0 << expect (Sym "EOF")) s with
  | Ok (value, _) -> value
  | Error msg -> raise (Failure msg)



(*
 * Rules Builders
 *)

(* let unary_prefix sym lbp = rule sym ~lbp *)
(*   ~nud: (consume sym >> *)
(*          parse_nud >>| *)
(*          fun x -> List [Atom sym; x]) *)

let unary_postfix sym =
  fun x -> return (List [Atom sym; x])

let binary_infix sym lbp = rule sym ~lbp
    ~led:(fun x -> consume sym >> parse_exp lbp >>=
          fun y -> return (List [Atom sym; x; y]))

let binary_infix_right sym lbp = rule sym ~lbp
  ~led:(fun x -> consume sym >> parse_exp (lbp - 1) >>=
        fun y -> return (List [Atom sym; x; y]))

(* Try to read the next led operator, if not defined, create a seq with
   the previous expression x and the next prefix y. *)
(* let newline sym lbp = rule sym ~lbp: (lbp - 1) *)
(*   ~led: (fun x -> advance >> parse_led (lbp - 1) x <|> (parse_exp (lbp - 1) >>= *)
(*          fun y -> return (List [Atom (Sym ";"); x; y]))) *)
(*   ~nud: (advance >> parse_exp (lbp - 1)) *)

let terminal sym = rule sym
    ~lbp:0
    ~nud:nud_error
    ~led:led_error

(* Delimiters separate expressions without affecting the parsing tree.
   They may appear only as binary infix and unary postfix symbols which means
   parsing null denotation results in an error.
   The left binding power for delimiters is low since they are strong separators. *)
let delimiter sym = rule sym
    ~lbp:0
    (* ~nud:nud_error *)
    (* ~led:led_error *)
    (* ~led:(fun exp -> consume sym >> return exp) *)

(* Groups behave like regular symbols and thus their left binding power has to be high
   (close or equal to default) to allow list parsing in `parse_led`. *)
let group start_sym end_sym =
  rule start_sym
    ~lbp:80
    ~nud:begin
      consume start_sym >>
      parse_exp 0 >>= fun exp ->
      consume end_sym >>
      return exp
    end

(* let closed start_sym end_sym = *)
(*   let sc = Scope.(define (delimiter end_sym) empty) in *)
(*   rule start_sym *)
(*     ~lbp:90 *)
(*     ~nud:begin *)
(*       consume start_sym >> *)
(*       push_scope sc >> *)
(*       parse_exp 0 >>= fun exp -> *)
(*       pop_scope >> *)
(*       return exp *)
(*     end *)

(* The default rule is essential for the correct parsing of atoms and lists.
   This rule's nud will create atomic expressions with the undefined symbols.
   And the led will create lists of expressions.
   The default rule has the highest left binding power. *)
let default sym = rule sym
    ~lbp:90
    ~led:begin fun prev_exp ->
      parse_exp 90 >>= fun next_exp ->
      return @ List (match prev_exp with
        | List xs -> List.append xs [next_exp]
        | atom    -> [atom; next_exp])
    end
    ~nud:(consume sym >> return (Atom sym))

