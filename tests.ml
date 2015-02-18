
open Foundation
open Lexicon
open Syntax
open Pratt
open Fold


let (~>) s =
    let r = parse ~input: (Lexing.from_string s)
                ~grammar: (grammar map) () in
    print ((bright_blue "-> ") ^ s);
    print (" = " ^ show_expr r)

let (==) s e =
    let r = parse ~input: (Lexing.from_string s)
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

let x       = Atom (Symbol "x")
let y       = Atom (Symbol "y")
let z       = Atom (Symbol "z")
let m xs    = Term (Symbol "module", xs)

let run () =
  (* ""             == epsilon; *)
  "5"            == Atom (Integer 5);

  ~> "a = 5"
  (* ~> "a = 5\n\t6\n\t9\n\t8\n\t2" *)

  (* "f x\n\t\ty"   == m [Term (Symbol "f", [x; y])]; *)
  (* "f x\ny"       == m [Term (Symbol "f", [x]); y] *)
  (* "f x\n\ty"     == m [Term (Symbol "f", [x; y])] *)
  (* "(x + y) z"    == Term (Symbol "+", [x; y; z]); *)
  (* ("f x y\n\tz") == Term (Symbol "f", [x; y; z]); *)

