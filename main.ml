
open Foundation
open Syntax
open Lexicon
open Parser
open Pratt

module Table = Map.Make(String)

let add_symbol name sym =
    Table.add name sym

let map =
    Table.empty
    |> add_symbol "`+" (infix 6)
    |> add_symbol "`*" (infix 7)
    |> add_symbol "`/" (infix 7)
    |> add_symbol "`=" (infix 1)
    |> add_symbol "`-" (infix 6)
    |> add_symbol "`++" (postfix 8)
    |> add_symbol "`!!" (postfix 8)
    |> add_symbol "`(" (initial 9)
    |> add_symbol "`)" (final 0)
    |> add_symbol "`atom" (atomic 9)
    |> add_symbol "`end" (final 0)
    |> add_symbol "`module" (block 0)

let grammar map tok =
    let tok_id = show_literal tok.value in
    let mk_sym = if Table.mem tok_id map
        then Table.find tok_id map
        else Table.find "`atom" map in
    mk_sym tok


let parse ~input ~grammar =
    let state  = { input; grammar; symbol = grammar start_token } in
    match run (parse_expr 0) state with
    | Ok (value, _) -> value
    | Error msg -> raise (Failure msg)


let (~>) s =
    let e = parse ~input: (Lexing.from_string s) ~grammar: (grammar map) in
    print ((bright_blue "-> ") ^ s);
    print (" = " ^ show_expr e)

let (==) s e =
    let r = parse ~input: (Lexing.from_string s) ~grammar: (grammar map) in
    let y = r = e in
    let i = if y then (bright_green "✓ ") else (bright_red "✗ ") in
    print_endline (format "%s %s %s" (bright_blue "->") (bright_white s) i);
    print_endline (format " = %s %s %s" (show_expr r) "::" (red "Expression"));
    if not y then
        (print_endline (format "\n  Expected: %s" (show_expr e));
       print_endline (format "    Actual: %s\n" (show_expr r)))
    else
        print_endline ""

let a         = Atom (Symbol "a")
let b         = Atom (Symbol "b")
let c         = Atom (Symbol "c")
let ( + ) a b = Term (Symbol "+", [a; b])
let ( ! ) a   = Term (Symbol "!", [a])
let ( ++ ) a  = Term (Symbol "++", [a])
let ( * ) a b = Term (Symbol "*", [a; b])
let f b       = Term (Symbol "f", [a])
let g a b c   = Term (Symbol "g", [a; b; c])
let h         = Atom (Symbol "h")
let m xs      = Term (Symbol "module", xs)

let () =
    ""              == m [];
    "a"             == m [a];
    "!a"            == m [! a];
    "a++"           == m [(++) a];
    "a + a"         == m [a + a];
    "a + a * a"     == m [(a + (a * a))];
    "f a"           == m [f a];
    "f a + a"       == m [(f a) + a];
    "f a + f a"     == m [(f a) + (f a)];
    "g a b b"       == m [g a b b];
    "h"             == m [h];
    "(a + a)"       == m [a + a];
    "(((a)))"       == m [a];
    ~> "g a (b + c) d";

    ~> "a = f c + f (x)";
    (* ~> "f g b" *)


    (* FIXME
        ~> "f 2 f 3 + 1"
        ~> "f !a - b) + 1";
    *)


