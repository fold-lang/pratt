
(* Basic calculator grammar. *)
let calc =
  let open Pratt.Rule in
  [token              (fun t   -> Int.parse (String.of_char t));
   prefix     '+'     (fun a   -> +a);
   infix   30 '+'     (fun a b -> a + b);
   prefix     '-'     (fun a   -> -a);
   infix   30 '-'     (fun a b -> a - b);
   infix   40 '*'     (fun a b -> a * b);
   infix   40 '/'     (fun a b -> a / b);
   between    '(' ')' (fun a   -> a);
   delimiter  ')']


(* Helper function that runs the parsing test with clac grammar. *)
let (==>) input expected =
  let iter = Iter.(filter (not << Char.Ascii.is_blank) (string input)) in
  let actual = Pratt.(run (parse calc) iter) in
  let printer = Nanotest.(result int (of_pp (Pratt.pp_error Fmt.char))) in
  Nanotest.(test printer ("\"" ^ input ^ "\"") ~actual ~expected)


let () =
  Nanotest.group "Test basic" [
    ""         ==> Error (Pratt.unexpected ());
    "1"        ==> Ok 1;
    "1+1"      ==> Ok 2;
    "1+-1"     ==> Ok 0;
    "(((0)))"  ==> Ok 0;
  ]

