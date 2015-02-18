
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
    led : expr -> (expr, state) parser;
    nud : (expr, state) parser }

(* -- Error Handling -- *)

let led_error = fun left ->
    get >>= fun { symbol } ->
        error (format "%s: %s cannot be used in infix position."
                      (show_location symbol.tok.location)
                      (show_literal symbol.tok.value))

let nud_error =
    get >>= fun { symbol } ->
        error (format "%s: %s cannot be used in prefix position."
                      (show_location symbol.tok.location)
                      (show_literal symbol.tok.value))

let symbol ?(lbp = 0) ?(led = led_error) ?(nud = nud_error) tok =
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


let rec parse_next rbp left =
    get >>= fun { symbol } ->
        trace (format "led: %s.lbp = %d, rbp = %d"
                      (show_literal symbol.tok.value) symbol.lbp rbp);
        if symbol.lbp > rbp
            then advance >> symbol.led left >>= fun next ->
                trace (format "led: * next = %s" (show_expr next));
                parse_next rbp next
            else (trace (format "led: ! (left = %s)" (show_expr left));
                  return left)

let parse_expr rbp =
    get >>= fun { symbol } ->
        advance >> symbol.nud >>= fun left ->
            trace (format "nud: * left = %s" (show_expr left));
            parse_next rbp left

let infix lbp tok = symbol tok
    ~lbp: lbp
    ~led: (fun x -> parse_expr lbp >>=
           fun y -> return (Term (tok.value, [x; y])))

let postfix lbp tok = symbol tok
    ~lbp: lbp
    ~led: (fun x -> return (Term (tok.value, [x])))

let atomic lbp tok = symbol tok
    ~lbp: lbp
    ~led: (function
           | Term (head, args) -> get >>= fun s ->
                trace (format "atomic(%s): s.level = %d, current_level = %d"
                              (show_literal tok.value) s.level
                              (current_column s.input));
                put s >> return (Term (head, args @ [Atom tok.value]))
           | Atom x -> error "Atomic expression cannot be applied.")
    ~nud: (return (Atom tok.value))

let parse_literal lit =
    get >>= fun { symbol } ->
    if (lit = symbol.tok.value)
        then advance
        else error (format "error: expected %s but got %s."
                     (show_literal lit) (show_literal symbol.tok.value))

let initial lbp tok = symbol tok
    ~lbp: lbp
    ~led: (fun x -> parse_expr 0 >>=
           fun y -> return (append_expr x y) << parse_literal (Symbol ")"))
    ~nud: (parse_expr 0 << parse_literal (Symbol ")"))

let final lbp tok = symbol tok
    ~lbp: lbp
    ~led: (fun x -> return (Atom (Symbol "wat?")))

let parse ~input ~grammar ?start () =
    let t0 = match start with
            | None -> (read_token input)
            | Some t -> t in
    let state  = { input; grammar;
                   level = (current_column input) + 1;
                   symbol = grammar t0 } in
    match run (parse_expr 0) state with
    | Ok (value, _) -> value
    | Error "Empty input" -> epsilon
    | Error msg -> raise (Failure msg)

