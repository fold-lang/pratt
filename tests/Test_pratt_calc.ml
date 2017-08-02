
module Char = Astring.Char

let implode l =
  let arr = Array.of_list l in
  String.init (Array.length arr) (Array.get arr)

let number =
  let open Pratt in
  many (range '0' '9') >>= fun xs ->
  match Int.parse (implode xs) with
  | Some n -> return n
  | None -> current >>= fun t -> error (Invalid_prefix t)

open Pratt

(* Basic calculator grammar. *)
let calc =
  [term number;
   prefix     '+'     (fun a   -> +a);
   infix   30 '+'     (fun a b -> a + b);
   prefix     '-'     (fun a   -> -a);
   infix   30 '-'     (fun a b -> a - b);
   infix   40 '*'     (fun a b -> a * b);
   infix   40 '/'     (fun a b -> a / b);
   between    '(' ')' (fun a   -> a);
   delimiter  ')']

(* Helper function that runs the parsing test with clac grammar. *)
let (==>) str expected =
  let input = Iter.filter (not << Char.Ascii.is_blank) (Iter.string str) in
  let actual = run (parse calc) input in
  let printer = Nanotest.(result int (of_pp (pp_error Fmt.char))) in
  Nanotest.(test ~verbose:true printer ("\"" ^ str ^ "\"") ~actual ~expected)


let () =
  Nanotest.group "Test basic" [
    "1"            ==> Ok 1;
    "x"            ==> Error (Invalid_prefix 'x');
    "+1"           ==> Ok 1;
    "+-+-1"        ==> Ok 1;
    "1 + 1"        ==> Ok 2;
    "100 + 300"    ==> Ok 400;
    "1 + -1"       ==> Ok 0;
    "1 + --1"      ==> Ok 2;
    "(((0)))"      ==> Ok 0;
    "2 + 2 * 2"    ==> Ok 6;
    "(2 + 2) * 2"  ==> Ok 8;
    ""             ==> Error (unexpected ());
    "/"            ==> Error (Invalid_prefix '/');
    "2 /"          ==> Error (unexpected ());
    "2 / -"        ==> Error (unexpected ());
    "2 2"          ==> Error (Invalid_infix '2');
    "2 ("          ==> Error (Invalid_infix '(');
  ]

