
open Pratt.Syntax
open Pratt.Lexer
open Pratt.Foundation
open Test_utils


let test_quotes () =
  let open Expr in
  print @ bright_magenta "-- Quotes";
  "`x"            == fn (sym "`") [sym "x"];
  "`f x"          == fn (sym "`") [sym "f"; sym "x"];
  "f `x `y"       == fn (sym "f") [fn (sym "`") [sym "x"]; fn (sym "`") [sym "y"]];
  "f `(x + y) `z" == fn (sym "f") [fn (sym "`") [fn (sym "+") [sym "x"; sym "y"]];
                                   fn (sym "`") [sym "z"]];
  print_newline ()


let () = begin
  test_quotes ();
end



