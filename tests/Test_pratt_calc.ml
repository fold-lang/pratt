open Local

let _1 = fst
module Stream = Pratt.Stream
module P = Pratt.Make(Char)
open P

(* Integer parser for char tokens. *)
let int g =
  some (range '0' '9') >>= fun (x, xs) ->
  return (Int.unsafe_of_string (String.implode (x :: xs)))

let rec fac = function
  | 0 | 1 -> 1
  | n -> n * fac (n - 1)

(* Basic calculator grammar. *)
let calc =
  grammar [
   term       int;
   null       '+'     (unary identity);
   left    30 '+'     (binary ( + ));
   null       '-'     (unary ( ~- ));
   left    30 '-'     (binary ( - ));
   left    40 '*'     (binary ( * ));
   left    40 '/'     (binary ( / ));
   postfix 70 '!'     (fun a   -> fac a);
   between    '(' ')' (fun a   -> a);
   delimiter  ')';
  ]


(* Basic string lexer (ignores blank characters). *)
let lexer str =
  Stream.of_string str |> Stream.reject Char.Ascii.is_blank

module T = Nanotest

(* Helper testing function that parses the input and checks the result. *)
let (==>) str expected =
  let actual = Result.map _1 (run (parse calc) (lexer str)) in
  let testable = T.(result int (testable (Fmt.of_to_string error_to_string))) in
  T.test testable str ~actual ~expected


let stream =
  T.testable ~equal:(fun a b -> Stream.(to_list a == to_list b))
    (Fmt.of_to_string (fun _ -> "<stream>"))

let (==>!) str expected =
  let actual = run (parse calc) (lexer str) in
  let testable = T.(result (pair int stream) (testable (Fmt.of_to_string error_to_string))) in
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
    "5!"           ==> Ok 120;
    "0!"           ==> Ok 1;
  ];

  T.group "Check errors" [
    "x"            ==> Error (unexpected_token 'x');
    ""             ==> Error (unexpected_end ());
    "/"            ==> Error (invalid_prefix '/');
    "2 /"          ==> Error (unexpected_end ());
    "2 / -"        ==> Error (unexpected_end ());
    "2 ("          ==>! Ok (2, Stream.of_list ['(']);
    (* XXX: Alternatively fail with unexpected ( *)
  ]

