
open Foundation
open Lexicon
open Syntax
open Parser



let rec parse_loop rbp left =
    get >>= fun { symbol } ->
        parse_led symbol rbp left <|> parse_nud symbol rbp left

and parse_nud sym rbp left = sym.nud => function
    | Some nud -> advance >> nud >>= fun x -> parse_loop rbp (append_expr left x)
    | None -> nud_error sym.tok

and parse_led sym rbp left = sym.led => function
    | Some led ->
        if sym.lbp > rbp
        then advance >> led left >>= parse_loop rbp
        else return left
    | None -> led_error sym.tok


let parse_expression rbp =
    get >>= fun { symbol } ->
        symbol.nud => function
        | None -> error (format "No nud for token `%s`." (show_token symbol.tok))
        | Some nud -> advance >> nud >>= parse_loop rbp

