
open Foundation
open Lexer
open Syntax
open Pratt
open Fold


let (~>) s =
    let r = parse ~lexer: (lexer_with_string "<TEST>" s)
                ~grammar: (grammar map) () in
    print ((bright_blue "-> ") ^ s);
    print (" = " ^ show_expr r)

let (==) s e =
    let r = parse ~lexer: (lexer_with_string "<TEST>" s)
                ~grammar: (grammar map) () in
    let y = r = e in
    let i = if y then (bright_green "✓ ") else (bright_red "✗ ") in
    print_endline (format "%s %s %s" (bright_blue "->") (bright_white s) i);
    print_endline (format " = %s %s %s" (show_expr r) "::" (red "Expr"));
    if not y then
        (print_endline (format "\n  Expected: %s" (show_expr e));
       print_endline (format "    Actual: %s\n" (show_expr r)))
    else
        ()

let x           = Atom (Symbol "x")
let y           = Atom (Symbol "y")
let z           = Atom (Symbol "z")
let add x y     = Term (Symbol "+", [x; y])
let mul x y     = Term (Symbol "*", [x; y])
let f x y       = Term (Symbol "f", [x; y])
let g x         = Term (Symbol "g", [x])
let def x y     = Term (Symbol "=", [x; y])

let run () =
  "5"             == Atom (Integer 5);
  "x + y"         == (add x y);
  "x + y * z"     == (add x (mul y z));
  "f x y"         == (f x y);
  "x; y; z"       == (seq [x; seq [y; z]]);
  "x + y; z"      == (seq [(add x y); z]);
  "x\ny\nz"       == (seq [x; seq [y; z]]);
  "x = y; z"      == (seq [(def x y); z]);
  "f x\n  y"      == Term (Symbol "f", [x; y]);
  "f x\ny"        == seq [Term (Symbol "f", [x]); y];
  "f x\n\ty"      == Term (Symbol "f", [x; y]);
  "f x y\n\tz"    == Term (Symbol "f", [x; y; z]);



