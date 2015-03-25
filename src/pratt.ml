
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
    led : (expr -> (expr, state) parser) option;
    nud : ((expr, state) parser) option }

(* -- Error Handling -- *)

let led_error tok =
  error (format "%s: %s cannot be used in infix position."
                (show_location tok.location)
                (show_literal tok.value))

let nud_error tok =
  error (format "%s: %s cannot be used in prefix position."
                (show_location tok.location)
                (show_literal tok.value))

let symbol ?(lbp = 0) ?led ?nud tok =
  { tok = tok;
    lbp = lbp;
    led = led;
    nud = nud }

let advance = get >>= fun s ->
    let token  = read_token s.lexer in
    let symbol = s.grammar token in
    put { s with symbol = symbol }

let consume = advance >> get

let rec parse_led rbp left =
  get >>= fun { symbol; level } ->
    match symbol.led with
    | Some led ->
      (trace (format "led[%d]: ~ tok = <%s>, col = %d, lbp = %d, rbp = %d"
                     level (show_literal symbol.tok.value)
                     symbol.tok.location.column symbol.lbp rbp);
      if symbol.lbp > rbp then
        (trace (format "led[%d]: > left = %s" level (show_expr left));
         advance >> led left >>= fun next ->
          (trace (format "led[%d]: * next = %s" level (show_expr next));
          parse_led rbp next))
      else
        (trace (format "led[%d]: ! left = %s" level (show_expr left));
         return left))
    | None -> nud_error symbol.tok

let parse_expr rbp =
  get >>= fun { symbol; level } ->
  match symbol.nud with
  | Some nud ->
    (trace (format "nud[%d]: ? tok = <%s>, col = %d, rbp = %d" level
                   (show_literal symbol.tok.value)
                   (symbol.tok.location.column) rbp);
    advance >> nud >>= fun left ->
      trace (format "nud[%d]: * left = %s" level (show_expr left));
      parse_led rbp left)
  | None -> nud_error symbol.tok

let parse_nud rbp =
  get >>= fun { symbol; level } ->
  match symbol.nud with
  | Some nud ->
    (trace (format "nud[%d]: ? tok = <%s>, col = %d, rbp = %d" level
                   (show_literal symbol.tok.value)
                   (symbol.tok.location.column) rbp);
    advance >> nud)
  | None -> nud_error symbol.tok

let infix lbp tok = symbol tok
    ~lbp: lbp
    ~led: (fun left  -> parse_expr lbp >>=
           fun right -> return (Term (tok.value, [left; right])))

let infix_r lbp tok = symbol tok
    ~lbp: lbp
    ~led: (fun prev -> parse_expr (lbp - 1) >>=
           fun next -> return (Term (tok.value, [prev; next])))

let postfix lbp tok = symbol tok
    ~lbp: lbp
    ~led: (fun x -> return (Term (tok.value, [x])))

let prefix tok =
  let e xs = (Term (tok.value, xs)) in
    symbol tok
      ~lbp: 1
      (* ~led: (fun e1 -> return (append_expr e1 e0)) *)
      ~nud: (many (parse_nud 0) >>= fun es -> return (e es))

let atomic lbp tok =
  let e0 = Atom tok.value in
    symbol tok
      ~lbp: lbp
      (* ~led: (fun e1 -> return (append_expr e1 e0)) *)
      ~nud: (return e0)

let parse_literal lit =
  get >>= fun { symbol } ->
  if (lit = symbol.tok.value)
      then advance
      else error (format "error: expected %s but got %s."
                   (show_literal lit) (show_literal symbol.tok.value))

let initial lbp tok = symbol tok
    ~lbp: lbp
    ~nud: (parse_expr 0 << parse_literal (Symbol ")"))

let final lbp tok = symbol tok
    ~lbp: lbp
    ~led: (fun x -> led_error tok)
    (* ~nud: (parse_expr 0) *)
    (* ~led: (fun x -> print "wat?"; return (Atom (Symbol "wat?"))) *)

let ternary_infix lbp tok = symbol tok
  ~lbp
  ~led: (fun x -> parse_expr 0 >>=
         fun y -> parse_literal (Symbol ":") >> parse_expr (lbp - 1) >>=
         fun z -> return (Term (tok.value, [x; y; z])))

let ternary_prefix lbp tok = symbol tok
  ~lbp
  ~nud: (parse_expr 0 >>=
           fun x -> parse_literal (Symbol "then") >> parse_expr 0 >>=
           fun y -> parse_literal (Symbol "else") >> parse_expr (lbp - 1) >>=
           fun z -> return (Term (tok.value, [x; y; z])))

let group lbp tok = symbol tok
  ~lbp
  ~nud: (parse_expr 0 >>= fun x ->
          (parse_literal (Symbol "EOL") >> parse_expr 0 >>=
           fun y -> parse_literal (Symbol ")") >> return (append_expr x y))
         <|>
          (parse_literal (Symbol ")") >> return x))

let parse ~lexer ~grammar ?start () =
    let t0 = match start with
            | None -> (read_token lexer)
            | Some t -> t in
    let s0 = { lexer;
               level = 1;
               grammar;
               symbol = grammar t0 } in
    match run (parse_expr 0) s0 with
    | Ok (value, _) -> value
    | Error msg -> raise (Failure msg)
