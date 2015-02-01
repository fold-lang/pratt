
open Foundation
open Lexicon
open Syntax
open Parser


let rec parse_loop rbp left =
    get >>= fun { symbol } ->
        if symbol.lbp > rbp
        then advance >> symbol.led left >>= parse_loop rbp
        else return left


let parse_expression rbp =
    get >>= fun { symbol } ->
        advance >> symbol.nud >>= parse_loop rbp

