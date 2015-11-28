
open Pratt.Syntax
open Pratt.Lexer
open Pratt.Foundation
open Test_utils


(* -- Test Groups -- *)

let test_literals () =
  print @ bright_magenta "-- Literals";
  "x"                   == sym "x";
  "5"                   == int 5;
  print_newline ()


let test_arithmetic_operators () =
  print @ bright_magenta "-- Arithmetic Operators";
  "x + y"      == fn (sym "+") [sym "x"; sym "y"];
  "-x"         == fn (sym "-") [sym "x"];
  "x + y * z"  == fn (sym "+") [sym "x"; fn (sym "*") [sym "y"; sym "z"]];
  "x + y * -z" == fn (sym "+") [sym "x"; fn (sym "*") [sym "y"; fn (sym "-") [sym "z"]]];
  print_newline ()


let test_lists () =
  (* TODO: Make a distinction between List (x f) and Func (f x y). *)
  print @ bright_magenta "-- Lists and Applications";
  "f x"             == fn (sym "f") [sym "x"];
  "(f) x"           == fn (sym "f") [sym "x"];
  "f (x)"           == fn (sym "f") [sym "x"];
  "f x y"           == fn (sym "f") [sym "x"; sym "y"];
  "(f x) y"         == fn (sym "f") [sym "x"; sym "y"];
  "f (f y)"         == fn (sym "f") [fn (sym "f") [sym "y"]];
  "(x # y) z"       == fn (sym "#") [sym "x"; sym "y"; sym "z"];
  "(f) (3)"         == fn (sym "f") [int 3];
  "(f 1) (g 2)"     == fn (sym "f") [int 1; fn (sym "g") [int 2]];
  "f x y + z"       == fn (sym "+") [fn (sym "f") [sym "x"; sym "y"]; sym "z"];
  "((((f) x) y) z)" == fn (sym "f") [sym "x"; sym "y"; sym "z"];
  "f (f (f 3))"     == fn (sym "f") [fn (sym "f") [fn (sym "f") [int 3]]];
  "(f (f (f 3)))"   == fn (sym "f") [fn (sym "f") [fn (sym "f") [int 3]]];
  "1 2 3 4"         == fn (int 1) [int 2; int 3; int 4];
  print_newline ()


let test_conditional () =
  print @ bright_magenta "-- Conditional";
  "if x then 1 else 0 end"  == fn (sym "if") [sym "x"; int 1; int 0];
  "if x then 1 end"         == fn (sym "if") [sym "x"; int 1];
  print_newline ()

let test_statements () =
  print @ bright_magenta "-- Statements";
  "x; y; z"    == seq (sym "x") (seq (sym "y") (sym "z"));
  "x + y; z"   == seq (fn (sym "+") [sym "x"; sym "y"]) (sym "z");
  "x = f y; 5" == fn (sym "=") [sym "x"; seq (fn (sym "f") [sym "y"]) (int 5)];
  print_newline ()

let test_groups () =
  print @ bright_magenta "-- Expression Groups";
  "(x)"                 == sym "x";
  "(((x)))"             == sym "x";
  "(x + y)"             == fn (sym "+") [sym "x"; sym "y"];
  "(x + y) * z"         == fn (sym "*") [fn (sym "+") [sym "x"; sym "y"]; sym "z"];
  "(x + (y + y)) * z"   == fn (sym "*") [fn (sym "+") [sym "x"; fn (sym "+") [sym "y"; sym "y"]]; sym "z"];
  "(f x y)"             == fn (sym "f") [sym "x"; sym "y"];
  print_newline ()

let test_blocks () =
  print @ bright_magenta "-- Blocks";
  "{2}"         == int 2;
  "{2 + 2}"     == fn (sym "+") [int 2; int 2];
  "f x {2 + 2}" == fn (sym "f") [sym "x"; fn (sym "+") [int 2; int 2]];
  "{x; y; z}"   == fn (sym ";") [sym "x"; fn (sym ";") [sym "y"; sym "z"]];
  "{x\ny\nz}"   == fn (sym ";") [sym "x"; fn (sym ";") [sym "y"; sym "z"]];
  ~>! "{}";
  ~>! "{\n}";
  print_newline ()

let test_newline_handling () =
  print @ bright_magenta "-- Newline Handling";
  "f x y\nz"    == fn (sym ";") [fn (sym "f") [sym "x"; sym "y"]; sym "z"];
  "x\ny"        == fn (sym ";") [sym "x"; sym "y"];
  "x +\ny"      == fn (sym "+") [sym "x"; sym "y"];
  "x\n! y"      == fn (sym ";") [sym "x"; fn (sym "!") [sym "y"]]; (* `!` as prefix only op. *)
  "f x y\nz"    == fn (sym ";") [fn (sym "f") [sym "x"; sym "y"]; sym "z"];
  "(x +\ny)"    == fn (sym "+") [sym "x"; sym "y"];
  "(f x\ny)"    == fn (sym "f") [sym "x"; sym "y"];
  "(f\nx y)"    == fn (sym "f") [sym "x"; sym "y"];
  print_newline ()

let test_edge_cases () =
  "1 + 1\n"            == fn (sym "+") [int 1; int 1];
  "{1 + 1\n}"          == fn (sym "+") [int 1; int 1];
  "{-1}"               == fn (sym "-") [int 1;];
  "{f x y\n}"          == fn (sym "f") [sym "x"; sym "y"];
  "{\nf x y\nx\ny\nz}" == seq (fn (sym "f") [sym "x"; sym "y"]) (seq (sym "x") (seq (sym "y") (sym "z")));
  "(x\n)"              == sym "x";
  "(\nx\n)"            == sym "x";
  ~>! "(x))";
  ~>! ")";
  ~>! "("; (* TODO: Error msg should be more explicit. Check it. *)
  ~>! "";
  print_newline ()

let test_quotes () =
  print @ bright_magenta "-- Quotes";
  "`x"            == fn (sym "`") [sym "x"];
  "`f x"          == fn (sym "`") [sym "f"; sym "x"];
  "f `x `y"       == fn (sym "f") [fn (sym "`") [sym "x"]; fn (sym "`") [sym "y"]];
  "f `(x + y) `z" == fn (sym "f") [fn (sym "`") [fn (sym "+") [sym "x"; sym "y"]];
                                   fn (sym "`") [sym "z"]];
  print_newline ()

let () = begin
  test_literals ();
  test_arithmetic_operators ();
  test_lists ();
  test_statements ();
  test_groups ();
  test_newline_handling ();
  test_blocks ();
  test_edge_cases ();
  test_quotes ();
  test_conditional ();
end



