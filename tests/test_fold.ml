
open Pratt.Syntax
open Pratt.Lexer
open Pratt.Foundation
open Test_utils


(* -- Test Groups -- *)

let test_literals () =
  let open Expr in
  print @ bright_magenta "-- Literals";
  "x"                   == sym "x";
  "5"                   == int 5;
  print_newline ()


let test_arithmetic_operators () =
  let open Expr in
  print @ bright_magenta "-- Arithmetic Operators";
  "x + y"      == call (sym "+") [sym "x"; sym "y"];
  "-x"         == call (sym "-") [sym "x"];
  "x + y * z"  == call (sym "+") [sym "x"; call (sym "*") [sym "y"; sym "z"]];
  "x + y * -z" == call (sym "+") [sym "x"; call (sym "*") [sym "y"; call (sym "-") [sym "z"]]];
  print_newline ()


let test_lists () =
  (* TODO: Make a distinction between List (x f) and Func (f x y). *)
  let open Expr in
  print @ bright_magenta "-- Lists and Applications";
  "f x"             == call (sym "f") [sym "x"];
  "(f) x"           == call (sym "f") [sym "x"];
  "f (x)"           == call (sym "f") [sym "x"];
  "f x y"           == call (sym "f") [sym "x"; sym "y"];
  "(f x) y"         == call (sym "f") [sym "x"; sym "y"];
  "f (f y)"         == call (sym "f") [call (sym "f") [sym "y"]];
  "(x # y) z"       == call (sym "#") [sym "x"; sym "y"; sym "z"];
  "(f) (3)"         == call (sym "f") [int 3];
  "(f 1) (g 2)"     == call (sym "f") [int 1; call (sym "g") [int 2]];
  "f x y + z"       == call (sym "+") [call (sym "f") [sym "x"; sym "y"]; sym "z"];
  "((((f) x) y) z)" == call (sym "f") [sym "x"; sym "y"; sym "z"];
  "f (f (f 3))"     == call (sym "f") [call (sym "f") [call (sym "f") [int 3]]];
  "(f (f (f 3)))"   == call (sym "f") [call (sym "f") [call (sym "f") [int 3]]];
  "1 2 3 4"         == call (int 1) [int 2; int 3; int 4];
  print_newline ()


let test_conditional () =
  let open Expr in
  print @ bright_magenta "-- Conditional";
  "if x then 1 else 0 end"  == call (sym "if") [sym "x"; int 1; int 0];
  "if x then 1 end"         == call (sym "if") [sym "x"; int 1];
  print_newline ()

let test_statements () =
  let open Expr in
  print @ bright_magenta "-- Statements";
  "x; y; z"    == seq (sym "x") (seq (sym "y") (sym "z"));
  "x + y; z"   == seq (call (sym "+") [sym "x"; sym "y"]) (sym "z");
  "x = f y; 5" == call (sym "=") [sym "x"; seq (call (sym "f") [sym "y"]) (int 5)];
  print_newline ()

let test_groups () =
  let open Expr in
  print @ bright_magenta "-- Exprression Groups";
  "(x)"                 == sym "x";
  "(((x)))"             == sym "x";
  "(x + y)"             == call (sym "+") [sym "x"; sym "y"];
  "(x + y) * z"         == call (sym "*") [call (sym "+") [sym "x"; sym "y"]; sym "z"];
  "(x + (y + y)) * z"   == call (sym "*") [call (sym "+") [sym "x"; call (sym "+") [sym "y"; sym "y"]]; sym "z"];
  "(f x y)"             == call (sym "f") [sym "x"; sym "y"];
  print_newline ()

let test_blocks () =
  let open Expr in
  print @ bright_magenta "-- Blocks";
  "{2}"         == int 2;
  "{2 + 2}"     == call (sym "+") [int 2; int 2];
  "f x {2 + 2}" == call (sym "f") [sym "x"; call (sym "+") [int 2; int 2]];
  "{x; y; z}"   == call (sym ";") [sym "x"; call (sym ";") [sym "y"; sym "z"]];
  "{x\ny\nz}"   == call (sym ";") [sym "x"; call (sym ";") [sym "y"; sym "z"]];
  ~>! "{}";
  ~>! "{\n}";
  print_newline ()

let test_newline_handling () =
  let open Expr in
  print @ bright_magenta "-- Newline Handling";
  "f x y\nz"       == call (sym ";") [call (sym "f") [sym "x"; sym "y"]; sym "z"];
  "x\ny"           == call (sym ";") [sym "x"; sym "y"];
  "x +\ny"         == call (sym "+") [sym "x"; sym "y"];
  "x\n! y"         == call (sym ";") [sym "x"; call (sym "!") [sym "y"]]; (* `!` as prefix only op. *)
  "f x y\nz"       == call (sym ";") [call (sym "f") [sym "x"; sym "y"]; sym "z"];
  "a = b - 1\n+ 4" == call (sym "=") [sym "a"; call (sym "+") [call (sym "-") [sym "b"; int 1]; int 4]];
  "(x +\ny)"       == call (sym "+") [sym "x"; sym "y"];
  "(f x\ny)"       == call (sym "f") [sym "x"; sym "y"];
  "(f\nx y)"       == call (sym "f") [sym "x"; sym "y"];
  print_newline ()

let test_edge_cases () =
  let open Expr in
  "1 + 1\n"            == call (sym "+") [int 1; int 1];
  "{1 + 1\n}"          == call (sym "+") [int 1; int 1];
  "{-1}"               == call (sym "-") [int 1;];
  "{f x y\n}"          == call (sym "f") [sym "x"; sym "y"];
  "{\nf x y\nx\ny\nz}" == seq (call (sym "f") [sym "x"; sym "y"]) (seq (sym "x") (seq (sym "y") (sym "z")));
  "(x\n)"              == sym "x";
  "(\nx\n)"            == sym "x";
  ~>! "(x))";
  ~>! ")";
  ~>! "("; (* TODO: Error msg should be more explicit. Check it. *)
  ~>! "";
  print_newline ()

let test_quotes () =
  let open Expr in
  print @ bright_magenta "-- Quotes";
  "`x"            == call (sym "`") [sym "x"];
  "`f x"          == call (sym "`") [sym "f"; sym "x"];
  "f `x `y"       == call (sym "f") [call (sym "`") [sym "x"]; call (sym "`") [sym "y"]];
  "f `(x + y) `z" == call (sym "f") [call (sym "`") [call (sym "+") [sym "x"; sym "y"]];
                                     call (sym "`") [sym "z"]];
  print_newline ()

let test_bugs () = begin
  let open Expr in
  print @ bright_magenta "-- Examples";
  "a = b - 1 + 4" == call (sym "=") [sym "a"; call (sym "+") [call (sym "-") [sym "b"; int 1]; int 4]];
  "a = b - 1\n+ 4" == call (sym "=") [sym "a"; call (sym "+") [call (sym "-") [sym "b"; int 1]; int 4]];
  "r = f a\nr" == seq (call (sym "=") [sym "r"; call (sym "f") [sym "a"]]) (sym "r");
  "do
     a = b - 1
       + 4
     r = f a
     r
   end" == seq
    (call (sym "=") [sym "a";
                   call (sym "+") [call (sym "-") [sym "b";
                                               int 1];
                                 int 4]])
    (seq (call (sym "=") [sym "r"; call (sym "f") [sym "a"]])
       (sym "r"))
end

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
  test_bugs ();
end



