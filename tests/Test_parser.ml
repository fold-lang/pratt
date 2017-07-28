
module P = Pratt
module T = Nanotest

let iter str =
  let next i =
    try
      Some (String.get str i, (i + 1))
    with Invalid_argument _ ->
      None in
  Iter.Iter (0, next)


(* Helper test function, tests a particular [parser] with a given [input]. *)
let test parser input expected =
  let message = "input: " ^ if input = "" then "<empty>" else input in
  let printer = T.result T.char (T.of_pp (P.pp_error Fmt.char)) in
  let actual  = P.run parser (iter input) in
  let () = match expected with
  | Error e -> P.error_to_string Fmt.char e |> print
  | _ -> () in
  T.test printer message ~expected ~actual

let test_error () =
  let (==>) = test P.(error Empty) in
  T.group "Parser.error" [
    ""    ==> Error P.Empty;
    "a"   ==> Error P.Empty;
    "abc" ==> Error P.Empty;
  ]

let test_expect () =
  let (==>) = test (P.expect 'a') in
  T.group "Parser.expect" [
    ""    ==> Error P.(Unexpected_end { expected = 'a' });
    "x"   ==> Error P.(Unexpected_token { expected = 'a'; actual = 'x'});
    "a"   ==> Ok 'a';
    "abc" ==> Ok 'a';
  ]

let test_exactly () =
  let (==>) (x, input) = test (P.exactly x) input in
  T.group "Parser.exactly" [
    ('x', "x") ==> Ok 'x';
    ('x', "y") ==> Error P.(Unexpected_token { expected = 'x'; actual = 'y' });
    ('x', "")  ==> Error P.(Unexpected_end { expected = 'x' });
  ]

let test_satisfy () =
  let (==>) = test (P.satisfy Char.Ascii.is_upper) in
  T.group "Parser.satisfy is_upper" [
    "A" ==> Ok 'A';
    ""  ==> Error P.(Failed_satisfy None);
    "0" ==> Error P.(Failed_satisfy (Some '0'));
    "a" ==> Error P.(Failed_satisfy (Some 'a'));
  ]

let test_any () =
  let (==>) = test P.any in
  T.group "Parser.any" [
    "x" ==> Ok 'x';
    "0" ==> Ok '0';
    "?" ==> Ok '?';
    ""  ==> Error P.(Failed_satisfy None);
  ]

let test_from () =
  let (==>) (options, input) = test (P.from options) input in
  T.group "Parser.from" [
    ([], "x")         ==> Error P.(Failed_satisfy (Some 'x'));
    (['x'], "x")      ==> Ok 'x';
    (['x'; 'y'], "y") ==> Ok 'y';
    (['x'; 'y'], "z") ==> Error P.(Failed_satisfy (Some 'z'));
    ([],  "")         ==> Error P.(Failed_satisfy None);
  ]

let () = begin
  test_error ();
  test_expect ();
  test_exactly ();
  test_satisfy ();
  test_any ();
  test_from ();
end

