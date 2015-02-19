
open Foundation
open Lexer
open Syntax
open Pratt
open Fold


let (~>) s =
    let r = parse ~lexer: (lexer_with_string s)
                ~grammar: (grammar map) () in
    print ((bright_blue "-> ") ^ s);
    print (" = " ^ show_expr r)

let (==) s e =
    let r = parse ~lexer: (lexer_with_string s)
                ~grammar: (grammar map) () in
    let y = r = e in
    let i = if y then (bright_green "✓ ") else (bright_red "✗ ") in
    print_endline (format "%s %s %s" (bright_blue "->") (bright_white s) i);
    print_endline (format " = %s %s %s" (show_expr r) "::" (red "Expression"));
    if not y then
        (print_endline (format "\n  Expected: %s" (show_expr e));
       print_endline (format "    Actual: %s\n" (show_expr r)))
    else
        print_endline ""

let x           = Atom (Symbol "x")
let y           = Atom (Symbol "y")
let z           = Atom (Symbol "z")
let add x y     = Term (Symbol "+", [x; y])
let mul x y     = Term (Symbol "*", [x; y])
let seq x y     = Term (Symbol ";", [x; y])
let f x y       = Term (Symbol "f", [x; y])
let def x y     = Term (Symbol "=", [x; y])

let run () =
  "5"             == Atom (Integer 5);
  "x + y"         == (add x y);
  "x + y * z"     == (add x (mul y z));
  "f x y"         == (f x y);
  "x; y; z"       == (seq (seq x y) z);
  "x = y; z"      == (def x (seq y z));

  "f x\n"         == (f x y);

  ~> "print msg"

  (* ~> "a = 5\n\t6\n\t9\n\t8\n\t2" *)

  (* "f x\n\t\ty"   == m [Term (Symbol "f", [x; y])]; *)
  (* "f x\ny"       == m [Term (Symbol "f", [x]); y] *)
  (* "f x\n\ty"     == m [Term (Symbol "f", [x; y])] *)
  (* "(x + y) z"    == Term (Symbol "+", [x; y; z]); *)
  (* ("f x y\n\tz") == Term (Symbol "f", [x; y; z]); *)

