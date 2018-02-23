open Local

let _1 = fst
module Stream = Pratt.Stream
module P = Pratt.Make(Lispy_lexer.Token)
open P

module E = struct
  type t = [ `sym of string | `add of t * t | `neg of t | `app of t list ]
  let rec pp formatter e =
    match e with
    | `sym a -> Format.fprintf formatter "%s" a
    | `add (a, b) -> Format.fprintf formatter "(%a + %a)" pp a pp b
    | `neg a -> Format.fprintf formatter "(- %a)" pp a
    | `app xs -> Format.fprintf formatter "(%a)" (Fmt.list ~sep:(Fmt.unit " ") pp) xs
  let equal = (=)
end

(* Integer parser for char tokens. *)
let parse_sym g =
  any >>= fun x -> advance >>= fun () -> return (`sym x)

let parse_term g =
  some begin
    current >>= fun token ->
    guard (not (Grammar.has_left token g)) >>= fun () ->
    parse_sym g
  end >>= fun (x, xs) ->
  if List.length xs = 0 then
    return x
  else
    return (`app (x :: xs))


(* Basic calculator grammar. *)
let calc =
  grammar [
   term       parse_term;
   left    30 "+"     (binary (fun a b -> `add (a, b)));
   null       "-"     (unary (fun a -> `neg a));
   between    "(" ")" (fun a   -> a);
  ]

let parser = parse calc

module T = Nanotest

(* Helper testing function that parses the input and checks the result. *)
let (==>) str expected =
  let actual = Result.map _1 (run parser (Lispy_lexer.(of_string str |> to_stream))) in
  let testable = T.(result (module E) (testable (Fmt.of_to_string error_to_string))) in
  T.test testable str ~actual ~expected


(* Tests *)
let () =
  T.group "Test basic" [
    "a"            ==> Ok (`sym "a");
    "(a)"          ==> Ok (`sym "a");
    "a + b"        ==> Ok (`add (`sym "a", `sym "b"));
    (* "a + -b"       ==> Ok (`add (`sym "a", `neg (`sym "b"))); *)
    (* "a + (b + c)"  ==> Ok (`add (`sym "a", `add (`sym "b", `sym "c"))); *)
    (* "(a + b) + c"  ==> Ok (`add (`add (`sym "a", `sym "b"), `sym "c")); *)
  ];

  (* T.group "Test list" [ *)
  (*   "a b"            ==> Ok (`app [`sym "a"; `sym "b"]); *)
  (* ]; *)


