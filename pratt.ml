
open Foundation
open Lexicon
open Syntax
open Parser


let parse_one_nud rbp handler =
    get >>= fun { symbol } ->
        match symbol.nud with
        | Some nud ->
            trace (format "nud: + tok = %s" (show_literal symbol.tok.value));
            advance >> nud >>= fun expr ->
                trace (format "one: * expr = %s" (show_expr expr));
                handler expr
        | None -> trace (format "nud: - tok = %s" (show_literal symbol.tok.value));
                  nud_error symbol.tok


let rec parse_alt rbp left =
    parse_led rbp left <|> parse_expr rbp

and parse_expr rbp = parse_one_nud rbp (parse_alt rbp)

and parse_led rbp left =
    get >>= fun { symbol } -> symbol.led => function
    | Some led -> trace (format "led: + tok = %s" (show_literal symbol.tok.value));
                  if symbol.lbp > rbp
                  then advance >> led left >>= fun expr ->
                    (trace (format "led: > expr = %s" (show_expr expr));
                     parse_alt rbp expr)
                  else return left
    | None -> trace (format "led: - tok = %s" (show_literal symbol.tok.value));
              led_error symbol.tok
