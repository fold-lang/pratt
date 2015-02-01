
open Foundation
open Syntax
open Lexicon
open Parser
open Pratt


let __start__ = { tok = start_token;
                  lbp = 0xFF;
                  nud = return empty_expr;
                  led = led_error start_token }

let __end__ = { tok = end_token;
                lbp = 0x00;
                nud = nud_error end_token;
                led = return }

let infix tok lbp =
    { tok = tok;
      lbp = lbp;
      led = (fun a -> parse_expression lbp >>=
             fun b -> return (List [Atom tok.value; a; b]));
      nud = nud_error tok }

let var tok =
    { tok = tok;
      lbp = 0x01;
      led = led_error tok;
      nud = return (Atom tok.value) }

let grammar tok =
    tok => function
    | { value = Symbol "*" } -> infix tok 0x50
    | { value = Symbol "+" } -> infix tok 0x40
    | { value = Symbol "a" } -> var tok
    | tok when tok = end_token -> __end__
    | tok -> raise (Failure (format "`%s" (show_token tok)))

let parse ~input ~grammar =
    let state  = { input; grammar; symbol = (grammar (read_token input)) } in
    match run (parse_expression 0) state with
    | Ok (value, _) -> value
    | Error msg -> raise (Failure msg)

    
let (~>) s =
    let e = parse ~input: (Lexing.from_string s) ~grammar in
    print ("-> " ^ s);
    print (" = " ^ show_expr e)

let (==) s e =
    let r = parse ~input: (Lexing.from_string s) ~grammar in
    let y = r = e in
    let i = if y then (bright_green "✓ ") else (bright_red "✗ ") in
    print_endline (format "%s %s %s" (bright_blue "->") (bright_white s) i);
    print_endline (format " = %s %s %s" (show_expr e) "::" (red "Expression"));
    if not y then
        (print_endline (format "\n  Expected: %s" (show_expr e));
       print_endline (format "    Actual: %s\n" (show_expr r)))
    else
        print_endline ""


let a = Atom (Symbol "a")
let ( + ) a b = List [Atom (Symbol "+"); a; b]
let ( * ) a b = List [Atom (Symbol "*"); a; b]

let () =
    "a" == a;
    "a + a" == a + a;
    "a + a * a" == (a + (a * a))


