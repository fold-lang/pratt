
open Foundation
open Lexer
open Syntax
open Parser

type state =
  { lexer   : lexer;
    grammar : token -> symbol;
    level   : int;
    symbol  : symbol }

and symbol =
  { tok : token;
    lbp : int;
    led : expr -> (expr, state) parser;
    nud : (expr, state) parser }

(* -- Error Handling -- *)

let led_error tok = fun left ->
  error (format "%s: %s cannot be used in infix position."
                (show_location tok.location)
                (show_literal tok.value))

let nud_error tok =
  error (format "%s: %s cannot be used in prefix position."
                (show_location tok.location)
                (show_literal tok.value))

let symbol ?(lbp = 0) ?led ?nud tok =
  let l, n = match led, nud with
            | None, None     -> led_error tok, nud_error tok
            | Some l, Some n -> l, n
            | Some l, None   -> l, nud_error tok
            | None, Some n   -> led_error tok, n
  in { tok = tok;
       lbp = lbp;
       led = l;
       nud = n }

let advance = get >>= fun s ->
    let token  = read_token s.lexer in
    let symbol = s.grammar token in
    put { s with symbol = symbol }

let consume = advance >> get


(* --- Layout --- *)

let with_indent indent p =
  get >>= fun s ->
    let curr_indent = s.level in
    put { s with level = indent } >>
    p >>= fun e ->
        put { s with level = curr_indent } >>
    return e

let use_indent indent p =
  get >>= fun s ->
    put { s with level = indent } >> p

let laidout p =
  get >>= fun s ->
    with_indent (current_token_column s.lexer) p

let indent_cmp cmp =
  get >>= fun s ->
    let curr_indent = s.level in
    let next_indent = current_token_column s.lexer in
    if (cmp curr_indent next_indent)
      then zero
      else error (format "Indentation does not match: curr = %d, next = %d"
                         curr_indent next_indent)

let indented = indent_cmp (>)
let align    = indent_cmp (=)

let block p =
  laidout (many (align >> p))

let rec parse_next rbp left =
    get >>= fun { symbol; level } ->
        trace (format "led[%d]: ~ tok = <%s>, col = %d, lbp = %d, rbp = %d" level
                      (show_literal symbol.tok.value) (symbol.tok.location.column) symbol.lbp rbp);
        if symbol.lbp > rbp
            then (trace (format "led[%d]: > left = %s" level (show_expr left));
                  advance >> symbol.led left >>= fun next ->
                  trace (format "led[%d]: * next = %s" level (show_expr next));
                  parse_next rbp next)
            else (trace (format "led[%d]: ! left = %s" level (show_expr left));
                  return left)

let parse_expr rbp =
    get >>= fun { symbol; level } ->
        trace (format "nud[%d]: ? tok = <%s>, col = %d, rbp = %d" level
                      (show_literal symbol.tok.value) (symbol.tok.location.column) rbp);
        advance >> symbol.nud >>= fun left ->
            trace (format "nud[%d]: * left = %s" level (show_expr left));
            parse_next rbp left

let infix lbp tok = symbol tok
    ~lbp: lbp
    ~led: (fun x -> parse_expr lbp >>=
           fun y -> return (Term (tok.value, [x; y])))

let infix_r lbp tok = symbol tok
    ~lbp: lbp
    ~led: (fun prev -> parse_expr (lbp - 1) >>=
           fun next -> return (Term (tok.value, [prev; next])))

let seq expr_list = (Term (Symbol ";", expr_list))

let indent_led indent expr = fun prev ->
  let parse_atomic =
      parse_next 0 expr >>= fun next -> return (seq [prev; next]) in
  get >>= fun st ->
    match indent <~> st.level with
      | `EQ -> parse_atomic
      | `GT -> return (append_expr prev expr)
      | `LT -> use_indent indent parse_atomic

let prefix tok =
    let expr = (Term (tok.value, [])) in
    symbol tok
      ~lbp: 1
      ~led: (indent_led tok.location.column expr)
      ~nud: (use_indent tok.location.column (return expr))

let postfix lbp tok = symbol tok
    ~lbp: lbp
    ~led: (fun x -> return (Term (tok.value, [x])))

let atomic lbp tok =
    let expr = Atom tok.value in
    symbol tok
      ~lbp: lbp
      ~led: (indent_led tok.location.column expr)
      ~nud: (return expr)

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

let parse ~lexer ~grammar ?start () =
    let t0 = match start with
            | None -> (read_token lexer)
            | Some t -> t in
    let s0 = { lexer;
               grammar;
               level = 1;
               symbol = grammar t0 } in
    match run (parse_expr 0) s0 with
    | Ok (value, _) -> value
    | Error msg -> raise (Failure msg)

let ignore tok = symbol tok
    ~nud: (parse_expr 0)
    ~led: (parse_next 0)
