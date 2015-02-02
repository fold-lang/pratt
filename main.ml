
open Foundation
open Syntax
open Lexicon
open Parser
open Pratt


let __start__ = { tok = start_token;
                  lbp = 0x00;
                  (* TODO: Instead of an `a` parse many. *)
                  nud = Some (parse_expression 0 >>= fun a ->
                                return (Term (Symbol "module", [a])));
                  led = None }

let __end__ = { tok = end_token;
                lbp = 0x00;
                nud = None;
                led = Some return }

let infix tok lbp =
    { tok = tok;
      lbp = lbp;
      led = Some (fun a -> parse_expression lbp >>=
                  fun b -> return (Term (tok.value, [a; b])));
      nud = None }

let var tok =
    { tok = tok;
      lbp = 0x00;
      led = None;
      nud = Some (return (Atom tok.value)) }


let left_paren tok =
    { tok = tok;
      lbp = 0x0;
      led = None;
      nud = Some (parse_expression 0 >>= fun a ->
                  advance >> return a) }


let grammar tok =
    tok => function
    | { value = Symbol "*" } -> infix tok 0x50
    | { value = Symbol "+" } -> infix tok 0x40
    | { value = Symbol "a" } -> var tok
    | { value = Symbol "b" } -> var tok
    | { value = Symbol "f" } -> var tok
    | { value = Symbol "g" } -> var tok
    | { value = Symbol "(" } -> left_paren tok
    | tok when tok = end_token -> __end__
    | tok -> raise (Failure (format "`%s" (show_token tok)))

let parse ~input ~grammar =
    let state  = { input; grammar; symbol = __start__ } in
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


let a         = Atom (Symbol "a")
let b         = Atom (Symbol "b")
let c         = Atom (Symbol "c")
let ( + ) a b = Term (Symbol "+", [a; b])
let ( * ) a b = Term (Symbol "*", [a; b])
let f b       = Term (Symbol "f", [a])
let g a b c   = Term (Symbol "g", [a; b; c])
let m es      = Term (Symbol "module", es)

let () =
    "a"         == m [a];
    "a + a"     == m [a + a];
    "a + a * a" == m [(a + (a * a))];
    "f a"       == m [f a];
    "f a + a"   == m [(f a) + a];
    "f a + f a" == m [(f a) + (f a)];
    "g a b b"   == m [g a b b];


