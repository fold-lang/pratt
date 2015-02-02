
open Foundation
open Syntax
open Lexicon
open Parser
open Pratt


let __start__ = { tok = start_token;
                  lbp = 0xFF;
                  nud = Some (return empty_expr);
                  led = None }

let __end__ = { tok = end_token;
                lbp = 0x00;
                nud = None;
                led = Some return }

let infix tok lbp =
    { tok = tok;
      lbp = lbp;
      led = Some (fun a -> parse_expression lbp >>=
                  fun b -> return (List [Atom tok.value; a; b]));
      nud = None }

let var tok =
    { tok = tok;
      lbp = 0xff;
      led = None;
      nud = Some (return (Atom tok.value)) }

let grammar tok =
    tok => function
    | { value = Symbol "*" } -> infix tok 0x50
    | { value = Symbol "+" } -> infix tok 0x40
    | { value = Symbol "a" } -> var tok
    | { value = Symbol "f" } -> var tok
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
let f a = List [Atom (Symbol "f"); a]

let () =
    "a" == a;
    "a + a" == a + a;
    "a + a * a" == (a + (a * a));
    "f a" == f a;
    "f a + a" == (f a) + a;
    "f a + f a" == (f a) + (f a);


