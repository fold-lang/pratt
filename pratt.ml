
open Foundation
open Lexicon
open Syntax
open Parser

type state =
  { input   : token_stream;
    grammar : token -> symbol;
    level   : int;
    symbol  : symbol }

and symbol =
  { tok : token;
    lbp : int;
    led : (expr -> (expr, state) parser) option;
    nud : (expr, state) parser option }

let symbol ?(lbp = 0) ?led ?nud tok =
  { tok = tok;
    lbp = lbp;
    led = led;
    nud = nud }

let advance = get >>= fun s ->
    let token  = read_token s.input in
    let symbol = s.grammar token in
    put { s with symbol = symbol }

let consume = advance >> get


(* --- Layout --- *)

let with_indent l p =
  get >>= fun s ->
    let curr = s.level in
    put { s with level = l } >>
    p >>= fun e ->
        put { s with level = curr } >>
    return e

let current_column lexbuf =
  Lexing.(lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)

let current_column lexbuf =
  Lexing.(lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)

let show_pos lexbuf =
  Lexing.(format "%s:%d:%d" lexbuf.lex_curr_p.pos_fname
                 lexbuf.lex_curr_p.pos_lnum
                 (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol))

let laidout p =
    get >>= fun s ->
      with_indent (current_column s.input) p

let indent_cmp cmp =
  get >>= fun s ->
    let curr = s.level in
    let next = current_column s.input in
    if (cmp curr next)
      then zero
      else error (format "Indentation does not match: curr = %d, next = %d"
                         curr next)

let indented = indent_cmp (>)
let align    = indent_cmp (=)

let block p =
  laidout (many (align >> p))


(* -- Error Handling -- *)

let led_error t =
    error (format "%s: symbol %s takes no arguments."
            (show_location t.location) (show_literal t.value))

let nud_error t =
    error (format "%s: symbol %s requires a left argument."
            (show_location t.location) (show_literal t.value))

let rec parse_alt rbp left =
    get >>= fun { symbol } ->
        trace (format "alt: (%s.lbp = %d, rbp = %d, left = %s)"
                      (show_literal symbol.tok.value) symbol.lbp rbp (show_expr left));
        if symbol.lbp > rbp
            then parse_next rbp left <|> parse_expr 0
            else (trace "alt: !"; return left)

and parse_expr rbp =
    get >>= fun { symbol } ->
        match symbol.nud with
        | Some nud ->
            trace (format "nud: + tok = %s" (show_literal symbol.tok.value));
            advance >> nud >>= fun expr ->
                trace (format "nud: * expr = %s" (show_expr expr));
                parse_alt rbp expr
        | None ->
            trace (format "nud: - tok = %s" (show_literal symbol.tok.value));
            nud_error symbol.tok

and parse_next rbp left =
    get >>= fun { symbol } ->
        match symbol.led with
        | Some led ->
            trace (format "led: + tok = %s" (show_literal symbol.tok.value));
            advance >> led left >>= fun expr ->
                trace (format "led: * expr = %s" (show_expr expr));
                parse_alt rbp expr
        | None ->
            trace (format "led: - tok = %s" (show_literal symbol.tok.value));
            led_error symbol.tok


let infix lbp = fun tok ->
  { tok = tok;
    lbp = lbp;
    led = Some (fun a -> parse_expr lbp >>=
                fun b -> return (Term (tok.value, [a; b])));
    nud = None }

let postfix lbp = fun tok ->
  { tok = tok;
    lbp = lbp;
    led = Some (fun x -> return (Term (tok.value, [x])));
    nud = None }

let atomic_led_parser tok = function
    | Term (head, args) ->
        get >>= fun s ->
            trace (format "atomic(%s): s.level = %d, current_level = %d"
                          (show_literal tok.value) s.level (current_column s.input));
            put s
            >> return (Term (head, args @ [Atom tok.value]))
    | Atom x ->
        error "Atomic expression cannot be applied."

let atomic lbp = fun tok ->
  { tok = tok;
    lbp = lbp;
    led = Some (atomic_led_parser tok);
    nud = Some (return (Atom tok.value)) }

let parse_literal lit =
    get >>= fun { symbol } ->
    if (lit = symbol.tok.value)
        then advance
        else error (format "error: expected %s but got %s."
                     (show_literal lit) (show_literal symbol.tok.value))

let initial lbp = fun tok ->
  { tok = tok;
    lbp = lbp;
    led = Some (fun prev -> parse_expr 0
                    >>= (fun e -> return (append_expr prev e))
                    << parse_literal (Symbol ")"));
    nud = Some (parse_expr 0 <<
                parse_literal (Symbol ")")) }

let final lbp = fun tok ->
  { tok = tok;
    lbp = lbp;
    led = Some (fun e -> return @@ Atom (Symbol ""));
    nud = None }

let parse ~input ~grammar ?start () =
    let t0 = match start with
            | None -> (read_token input)
            | Some t -> t in
    let state  = { input; grammar;
                   level = (current_column input) + 1;
                   symbol = grammar t0 } in
    match run (parse_expr 0) state with
    | Ok (value, _) -> value
    | Error msg -> raise (Failure msg)

