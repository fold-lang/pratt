
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

let (<?>) p label = fun s ->
    match p s with
    | Error _ -> Error ("%s: expected %s but found %s." %%%
                  (show_location s.symbol.tok.location, label,
                   show_literal s.symbol.tok.value))
    | ok -> ok


let symbol ?(lbp = 0) ?led ?nud tok =
  { tok = tok;
    lbp = lbp;
    led = led;
    nud = nud }

let inspect_token =
 inspect (fun {symbol} -> print $ "tok: %s" % (show_literal symbol.tok.value))

let advance = get >>= fun s ->
    let token  = read_token s.lexer in
    let symbol = s.grammar token in
    put { s with symbol = symbol }

let consume x =
  satisfy (fun s -> s.symbol.tok.value = x) <?> (show_literal x)
  >> advance

let rec parse_next rbp left =
  get >>= fun { symbol } ->
    match symbol.led with
    | Some led ->
      (trace (format "led: ~ tok = <%s>, col = %d, lbp = %d, rbp = %d"
                     (show_literal symbol.tok.value)
                     symbol.tok.location.column symbol.lbp rbp);
      if symbol.lbp > rbp then
        (trace (format "led: > left = %s" (show_expr left));
         advance >> led left >>= fun next ->
          (trace (format "led: * next = %s" (show_expr next));
          parse_next rbp next))
      else
        (trace (format "led: ! left = %s" (show_expr left));
         return left))
    | None -> led_error symbol.tok

let parse_nud rbp =
  get >>= fun { symbol } ->
  match symbol.nud with
  | Some nud ->
    (trace (format "nud: ? tok = <%s>, col = %d, rbp = %d"
                   (show_literal symbol.tok.value)
                   (symbol.tok.location.column) rbp);
    advance >> nud)
  | None -> nud_error symbol.tok

let parse_expr rbp =
  parse_nud rbp >>= fun left ->
    trace (format "exp: * left = %s" (show_expr left));
    parse_next rbp left

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
  let ex xs = (Term (tok.value, xs)) in
    symbol tok
      ~nud: (many (parse_nud 0) >>= fun expr_list ->
              return (ex expr_list))

let atomic lbp tok =
  let e0 = Atom tok.value in
    symbol tok
      ~lbp: lbp
      ~nud: (return e0)

let initial lbp tok = symbol tok
    ~lbp: lbp
    ~nud: (parse_expr 0 << consume (Symbol ")"))

let final lbp tok = symbol tok
    ~lbp: lbp
    ~led: return

let ternary_infix lbp tok = symbol tok
  ~lbp
  ~led: (fun x -> parse_expr 0 >>=
         fun y -> consume (Symbol ":") >> parse_expr (lbp - 1) >>=
         fun z -> return (Term (tok.value, [x; y; z])))

let ternary_prefix lbp tok = symbol tok
  ~lbp
  ~nud: (parse_expr 0 >>=
           fun x -> consume (Symbol "then") >> parse_expr 0 >>=
           fun y -> consume (Symbol "else") >> parse_expr (lbp - 1) >>=
           fun z -> return (Term (tok.value, [x; y; z])))

let group lbp tok = symbol tok
  ~lbp
  ~nud: (parse_expr 0 >>= fun x -> consume (Symbol ")") >> return x)

let group_2 initial body final =
  consume initial >> body >> consume final

let block lbp tok = symbol tok
  ~lbp
  ~nud: (parse_expr 0)

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
