open Local
open Pratt

(* Integer parser for char tokens. *)
let int =
  some (range '0' '9') >>= fun (x, xs) ->
  return (Int.force_of_string (String.implode (x :: xs)))

let rec fac = function
  | 0 | 1 -> 1
  | n -> n * fac (n - 1)

(* Basic calculator grammar. *)
let calc =
  [term               int;
   prefix     '+'     (fun a -> a);
   infix   30 '+'     (fun a b -> a + b);
   prefix     '-'     (fun a -> -a);
   infix   30 '-'     (fun a b -> a - b);
   infix   40 '*'     (fun a b -> a * b);
   infix   40 '/'     (fun a b -> a / b);
   between    '(' ')' (fun a -> a);
   delimiter  ')';
   postfix 70 '!'     (fun a -> fac a);
   prefix     'f'     (fun a -> fac a);
  ]

(* Basic string lexer (ignores blank characters). *)
let lexer =
  Iter.string >> Iter.reject Char.Ascii.is_blank

module T = Nanotest

(* Helper testing function that parses the input and checks the result. *)
let (==>) str expected =
  let actual = run (parse calc) (lexer str) in
  let testable = T.(result int (testable (Fmt.of_to_string (error_to_string Fmt.char)))) in
  T.test testable str ~actual ~expected

(* Tests *)
let () =
  T.group "Test basic" [
    "1"            ==> Ok 1;
    "+1"           ==> Ok 1;
    "+-+-1"        ==> Ok 1;
    "1 + 1"        ==> Ok 2;
    "100 + 300"    ==> Ok 400;
    "1 + -1"       ==> Ok 0;
    "1 + --1"      ==> Ok 2;
    "(((0)))"      ==> Ok 0;
    "2 + 2 * 2"    ==> Ok 6;
    "(2 + 2) * 2"  ==> Ok 8;
  ];

  T.group "Factorial" [
    "f 5"          ==> Ok 120;
    "5!"           ==> Ok 120;
  ];

  T.group "Check errors" [
    "x"            ==> Error (unexpected_token 'x');
    ""             ==> Error (unexpected_end ());
    "/"            ==> Error (unexpected_token '/');
    "2 /"          ==> Error (unexpected_end ());
    "2 / -"        ==> Error (unexpected_end ());
    "2 ("          ==> Error (invalid_infix '(');
  ]

