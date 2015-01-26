
open Foundation
open Lexicon
open Syntax
open Parser


let rec parse_next rbp a =
    get >>= fun { symbol } ->
        let led = if symbol.lbp > rbp
                  then symbol.led a >>= fun b -> parse_next rbp b
                  else return a
        and nud = symbol.nud >>= fun b -> parse_next rbp (append_expr a b) in
        advance >> (led <|> nud)

let parse_expression rbp =
    get >>= fun { symbol } ->
        advance >> symbol.nud >>= parse_next rbp

let parse ~input ~grammar =
    let token  = read_token input in
    let symbol = grammar token in
    let state  = { input; grammar; symbol = symbol } in
    let result = run (return Syntax.empty_expr) state in
    match result with
    | Ok (value, _) -> value
    | Error msg -> raise (Failure msg)



(* let (==) s e =
    let r = parse s in
    let y = r = e in
    let i = if y then (bright_green "✓ ") else (bright_red "✗ ") in
    print_endline (format "%s %s %s" (bright_blue "->") (bright_white s) i);
    print_endline (format " = %s %s %s" (Expression.show e) "::" (red "Expression"));
    if not y then
        (print_endline (format "\n  Expected: %s" (Expression.show e));
       print_endline (format "    Actual: %s\n" (Expression.show r)))
    else
        print_endline ""
 *)
