
open Fold.Syntax
open Fold.Lexer
open Fold.Foundation
open Test_utils


(* -- Helper Definitions -- *)

let sym x = Atom (Symbol x)
let str x = Atom (String x)
let int x = Atom (Integer x)
let seq e1 e2 = Term (Symbol ";", [e1; e2])
let term head args = Term (Symbol head, args)

let x           = Atom (Symbol "x")
let y           = Atom (Symbol "y")
let z           = Atom (Symbol "z")
let add x y     = Term (Symbol "+", [x; y])
let neg x       = Term (Symbol "-", [x])
let mul x y     = Term (Symbol "*", [x; y])
let f x y       = Term (Symbol "f", [x; y])
let g x         = Term (Symbol "g", [x])
let def x y     = Term (Symbol "=", [x; y])


(* -- Test Groups -- *)

let test_literals () =
  print $ bright_white "-- Literals";
  "x"                   == x;
  "5"                   == int 5;
  print_newline ()


let test_arithmetic_operators () =
  print $ bright_white "-- Arithmetic Operators";
  "x + y"      == (add x y);
  "-x"         == (neg x);
  "x + y * z"  == (add x (mul y z));
  "x + y * -z" == (add x (mul y (neg z)));
  print_newline ()


let test_function_application () =
  print $ bright_white "-- Function Application";
  "f x y"      == (f x y);
  "f x y + z"  == (add (f x y) z);
  !! "x f ";
  print_newline ()


let test_conditional () =
  print $ bright_white "-- Conditional";
  "if x then 1 else 0"   == Term (Symbol "if:then:else", [x; int 1; int 0]);
  "if\nx then 1 else 0"  == Term (Symbol "if:then:else", [x; int 1; int 0]);
  print_newline ()


let test_groups () =
  print $ bright_white "-- Groups";
  "(x)"                 == x;
  "(((x)))"             == x;
  "(x + y)"             == (add x y);
  "(x + y) * z"         == (mul (add x y) z);
  "(x + (y + y)) * z"   == (mul (add x (add y y)) z);
  "(f x y)"             == (f x y);
  !! "(";
  !! "(x))";
  print_newline ()

let test_blocks () =
  print $ bright_white "-- Blocks";
  "{2}"           == (int 2);
  "{2 + 2}"       == (add (int 2) (int 2));
  "f x {2 + 2}"   == (f x (add (int 2) (int 2)));
  "{x\ny\nz}"     == (seq x (seq y z));
  !! "{}";
  !! "{\n}";
  print_newline ()

let test_sequences () =
  print $ bright_white "-- Sequences";
  "x; y; z"          == (seq x (seq y z));
  "x + y; z"         == (seq (add x y) z);
  "x = f y z; 5"     == (def x (seq (f y z) (int 5)));
  print_newline ()

let test_newline_handling () =
  print $ bright_white "-- Newline Handling";
  "f x y\nz"    == (seq (f x y) z);
  "x\ny"        == (seq x y);
  "x +\ny"      == (add x y);
  "x\n- y"      == (seq x (neg y));
  "f x y\nz"    == (seq (f x y) z);
  "(x +\ny)"    == (add x y);
  "(f x\ny)"    == (f x y);
  "(f\nx y)"    == (f x y);
  print_newline ()

let test_edge_cases () =
  "1 + 1\n"            == (add (int 1) (int 1)) ;
  "{1 + 1\n}"          == (add (int 1) (int 1));
  "{-1}"               == (neg (int 1));
  "{f x y\n}"          == (f x y);
  "{\nf x y\nx\ny\nz}" == (seq (f x y) (seq x (seq y z)));
  "(x\n)"              == x;
  "(\nx\n)"            == x;
  !! ")";
  !! "(";
  !! "(x + y) 3";
  print_newline ()

let run () =
  test_literals ();
  test_arithmetic_operators ();
  test_function_application ();
  test_sequences ();
  test_newline_handling ();
  test_groups ();
  test_blocks ();
  test_edge_cases ();

  ()


