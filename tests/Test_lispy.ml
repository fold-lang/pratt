open Local


let inspect pp =
  Format.fprintf Fmt.stderr "@[%a@.@]" pp

let log fmt =
  Format.kfprintf (fun f -> Format.pp_print_newline f ()) Fmt.stderr fmt


let _1 = fst
module Stream = Pratt.Stream
module P = Pratt.Make(Lispy_lexer.Token)
module G = P.Grammar
let (>>=) = P.(>>=)

module E = struct
  type t = [ `sym of string | `add of t * t | `sub of t * t | `neg of t | `seq of t list ]
  let rec pp formatter e =
    match e with
    | `sym a -> Format.fprintf formatter "%s" a
    | `add (a, b) -> Format.fprintf formatter "(%a + %a)" pp a pp b
    | `sub (a, b) -> Format.fprintf formatter "(%a - %a)" pp a pp b
    | `neg a -> Format.fprintf formatter "(- %a)" pp a
    | `seq xs -> Format.fprintf formatter "(%a)" (Fmt.list ~sep:(Fmt.unit " ") pp) xs
  let equal = (=)
end

let parse g =
  let left =
    P.some begin
      P.current >>= fun token ->
      P.nud 0 g
    end >>= fun (x, xs) ->
    if List.length xs = 0 then
      P.return x
    else
      P.return (`seq (x :: xs)) in
  left >>= P.led 0 g

let parse_term g =
  P.any >>= fun x ->
  P.return (`sym x)

let parse_add g a =
  P.advance >>= fun () ->
  parse g >>= fun b ->
  P.return (`add (a, b))

let parse_sub g a =
  P.advance >>= fun () ->
  parse g >>= fun b ->
  P.return (`sub (a, b))

let parse_neg g =
  P.advance >>= fun () ->
  parse g >>= fun a ->
  P.return (`neg a)

let parse_group g =
  P.advance >>= fun () ->
  parse g >>= fun a ->
  P.consume ")" >>= fun () ->
  P.return a

(* Basic calculator grammar. *)
let lispy =
  P.grammar P.[
    term               parse_term;
    left    30 "+"     parse_add;
    left    30 "-"     parse_sub;
    null       "-"     parse_neg;
    null       "("     parse_group;
    delimiter  ")"
  ]

let parser = parse lispy

module T = Nanotest

(* Helper testing function that parses the input and checks the result. *)
let (==>) str expected =
  let actual = Result.map _1 (P.run parser (Lispy_lexer.(of_string str |> to_stream))) in
  let testable = T.(result (module E) (testable (Fmt.of_to_string P.error_to_string))) in
  T.test testable str ~actual ~expected


(* Tests *)
let () =
  let (a, b, c, d, e) =
    `sym "a", `sym "b", `sym "c", `sym "d", `sym "e" in
  let (+) a b = `add (a, b) in
  let (~-) a = `neg a in
  let seq l = `seq l in

  T.group "Test basic" [
    "a"               ==> Ok a;
    "a + b"           ==> Ok (a + b);
    "-a"              ==> Ok ~-a;
    "a b"             ==> Ok (seq [a; b]);
    "a b c"           ==> Ok (seq [a; b; c]);
    "a b + c"         ==> Ok (seq [a; b] + c);
    "a + b c"         ==> Ok (a + seq [b; c]);
    "(a + b) (c + d)" ==> Ok (seq [a + b; c + d]);
    "(a b) (c d)"     ==> Ok (seq [seq [a; b]; seq [c; d]]);
  ];

  let def f args body =
    seq [`sym "def"; f; seq args; body] in
  let let' bindings body =
    seq [`sym "let"; seq bindings; body] in
  let lambda args body =
    seq [`sym "lambda"; seq args; body] in

  let ex1_str = {|
      (def a (a b c)
        (let
          ((d (a + b))
           (e (-c)))
         ((lambda (a b) (a + e)) d)))
    |} in
  let ex1_expr =
    def a [a; b; c]
      (let' [seq [d; a + b];
             seq [e; ~-c]]
         (seq [lambda [a; b] (a + e); d])) in

  T.group "Compound" [
    ex1_str ==> Ok ex1_expr;
    ex1_str ^ ex1_str ==> Ok (seq [ex1_expr; ex1_expr])
  ]


