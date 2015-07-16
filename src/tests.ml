
open Fold.Syntax
open Fold.Lexer
open Fold.Foundation
open Test_utils


(* -- Helper Definitions -- *)

let sym x = Atom (Sym x)
let str x = Atom (Str x)
let int x = Atom (Int x)

(* -- Test Groups -- *)

let test_literals () =
  print @ bright_magenta "-- Literals";
  "x"                   == sym "x";
  "5"                   == int 5;
  print_newline ()


let test_arithmetic_operators () =
  print @ bright_magenta "-- Arithmetic Operators";
  "x + y"      == List [sym "+"; sym "x"; sym "y"];
  "-x"         == List [sym "-"; sym "x"];
  "x + y * z"  == List [sym "+"; sym "x"; List [sym "*"; sym "y"; sym "z"]];
  "x + y * -z" == List [sym "+"; sym "x"; List [sym "*"; sym "y"; List [sym "-"; sym "z"]]];
  print_newline ()


let test_lists () =
  (* TODO: Make a distinction between List (x f) and Func (f x y). *)
  print @ bright_magenta "-- Lists and Applications";
  "f x"             == List [sym "f"; sym "x"];
  "(f) x"           == List [sym "f"; sym "x"];
  "f (x)"           == List [sym "f"; sym "x"];
  "f x y"           == List [sym "f"; sym "x"; sym "y"];
  "(f x) y"         == List [sym "f"; sym "x"; sym "y"];
  "f (f y)"         == List [sym "f"; List [sym "f"; sym "y"]];
  "(x # y) z"       == List [sym "#"; sym "x"; sym "y"; sym "z"];
  "(f) (3)"         == List [sym "f"; int 3];
  "(f 1) (g 2)"     == List [sym "f"; int 1; List [sym "g"; int 2]];
  "f x y + z"       == List [sym "+"; List [sym "f"; sym "x"; sym "y"]; sym "z"];
  "((((f) x) y) z)" == List [sym "f"; sym "x"; sym "y"; sym "z"];
  "f (f (f 3))"     == List [sym "f"; List [sym "f"; List [sym "f"; int 3]]];
  "(f (f (f 3)))"   == List [sym "f"; List [sym "f"; List [sym "f"; int 3]]];
  "1 2 3 4"         == List [int 1; int 2; int 3; int 4];
  print_newline ()


let test_conditional () =
  print @ bright_magenta "-- Conditional";
  "if x then 1 else 0"   == List [sym "if"; sym "x"; int 1; int 0];
  print_newline ()

let test_statements () =
  print @ bright_magenta "-- Statements";
  "x; y; z"    == List [sym ";"; sym "x"; List [sym ";"; sym "y"; sym "z"]];
  "x + y; z"   == List [sym ";"; List [sym "+"; sym "x"; sym "y"]; sym "z"];
  "x = f y; 5" == List [sym "="; sym "x"; List [sym ";"; List [sym "f"; sym "y"]; int 5]];
  print_newline ()

let test_groups () =
  print @ bright_magenta "-- Groups";
  "(x)"                 == sym "x";
  "(((x)))"             == sym "x";
  "(x + y)"             == List [sym "+"; sym "x"; sym "y"];
  "(x + y) * z"         == List [sym "*"; List [sym "+"; sym "x"; sym "y"]; sym "z"];
  "(x + (y + y)) * z"   == List [sym "*"; List [sym "+"; sym "x"; List [sym "+"; sym "y"; sym "y"]]; sym "z"];
  "(f x y)"             == List [sym "f"; sym "x"; sym "y"];
  print_newline ()

let test_blocks () =
  print @ bright_magenta "-- Blocks";
  "{2}"           == int 2;
  "{2 + 2}"       == List [sym "+"; int 2; int 2];
  "f x {2 + 2}"   == List [sym "f"; sym "x"; List [sym "+"; int 2; int 2]];
  (* "{x\ny\nz}"     == (list x (list y z)); *)
  ~>! "{}";
  ~>! "{\n}";
  print_newline ()

(* let test_newline_handling () = *)
(*   print @ bright_magenta "-- Newline Handling"; *)
(*   "f x y\nz"    == (list (f x y) z); *)
(*   "x\ny"        == (list x y); *)
(*   "x +\ny"      == (add x y); *)
(*   "x\n- y"      == (list x (neg y)); *)
(*   "f x y\nz"    == (list (f x y) z); *)
(*   "(x +\ny)"    == (add x y); *)
(*   "(f x\ny)"    == (f x y); *)
(*   "(f\nx y)"    == (f x y); *)
(*   print_newline () *)

let test_edge_cases () =
  "1 + 1\n"            == List [sym "+"; int 1; int 1];
  "{1 + 1\n}"          == List [sym "+"; int 1; int 1];
  "{-1}"               == List [sym "-"; int 1;];
  "{f x y\n}"          == List [sym "f"; sym "x"; sym "y"];
  (* "{\nf x y\nx\ny\nz}" == (list (f x y) (list x (list y z))); *)
  "(x\n)"              == sym "x";
  "(\nx\n)"            == sym "x";
  ~>! "(x))";
  ~>! ")";
  ~>! "(";
  ~>! "";
  print_newline ()

let run () =
  test_literals ();
  test_arithmetic_operators ();
  test_lists ();
  test_statements ();
  test_groups ();
  (* test_newline_handling (); *)
  (* test_blocks (); *)
  (* test_edge_cases (); *)

  ()


