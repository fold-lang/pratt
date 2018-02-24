open Local


let inspect pp =
  Format.fprintf Fmt.stderr "@[%a@.@]" pp

let log fmt =
  Format.kfprintf (fun f -> Format.pp_print_newline f ()) Fmt.stderr fmt


let _1 = fst
module Stream = Pratt.Stream
module P = Pratt.Make(Lispy_lexer.Token)
open P

module E = struct
  type t = [ `sym of string | `add of t * t | `neg of t | `seq of t list ]
  let rec pp formatter e =
    match e with
    | `sym a -> Format.fprintf formatter "%s" a
    | `add (a, b) -> Format.fprintf formatter "(%a + %a)" pp a pp b
    | `neg a -> Format.fprintf formatter "(- %a)" pp a
    | `seq xs -> Format.fprintf formatter "(%a)" (Fmt.list ~sep:(Fmt.unit " ") pp) xs
  let equal = (=)
end

(* Integer parser for char tokens. *)
let parse_sym g =
  any >>= fun x ->
  return (`sym x)

(* Basic calculator grammar. *)
let lispy =
  grammar [
   term       parse_sym;
   left    30 "+"     (binary (fun a b -> `add (a, b)));
   null       "-"     (unary (fun a -> `neg a));
   between    "(" ")" (fun a   -> a);
   delimiter  ")"
  ]

let parse_some' grammar =
  some begin
    current >>= fun token ->
    guard (not (Grammar.has_left token grammar)) >>= fun () ->
    nud 0 grammar
  end

let parser =
  let left =
    parse_some lispy >>= fun (x, xs) ->
    if List.length xs = 0 then
      return x
    else
      return (`seq (x :: xs))
  in
    left >>= led 0 lispy

module T = Nanotest

(* Helper testing function that parses the input and checks the result. *)
let (==>) str expected =
  let actual = Result.map _1 (run parser (Lispy_lexer.(of_string str |> to_stream))) in
  let testable = T.(result (module E) (testable (Fmt.of_to_string error_to_string))) in
  T.test testable str ~actual ~expected


(* Tests *)
let () =
  let (a, b, c, d, e) =
    `sym "a", `sym "b", `sym "c", `sym "d", `sym "e" in
  let (+) a b = `add (a, b) in
  let seq l = `seq l in

  T.group "Test basic" [
    "a"            ==> Ok a;
    "(a)"          ==> Ok a;
    "a + b"        ==> Ok (a + b);
    "a + -b"       ==> Ok (`add (`sym "a", `neg (`sym "b")));
    "a + (b + c)"  ==> Ok (`add (`sym "a", `add (`sym "b", `sym "c")));
    "(a + b) + c"  ==> Ok (`add (`add (`sym "a", `sym "b"), `sym "c"));
  ];

  T.group "Test list" [
    "a b"            ==> Ok (seq [a; b]);
    "a b c"          ==> Ok (seq [a; b; c]);
    "a b + c"        ==> Ok (seq [a; b] + c);
  ];


