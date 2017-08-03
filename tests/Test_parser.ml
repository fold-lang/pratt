open Astring
module P = Pratt
module T = Nanotest


(* Helper test function, tests a particular [parser] with a given [input]. *)
let test parser input expected =
  let printer = T.result T.char (T.testable (P.pp_error Fmt.char)) in
  let actual  = P.run parser (Iter.string input) in
  let message = "\"" ^ input ^ "\"" in
  T.test ~verbose:false printer message ~expected ~actual

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
    ""    ==> Error P.(unexpected_end ~expected:'a' ());
    "x"   ==> Error P.(unexpected_token ~expected:'a' 'x');
    "a"   ==> Ok 'a';
    "abc" ==> Ok 'a';
  ]

let test_exactly () =
  let (==>) (x, input) = test (P.exactly x) input in
  T.group "Parser.exactly" [
    ('x', "x") ==> Ok 'x';
    ('x', "y") ==> Error P.(unexpected_token ~expected:'x' 'y');
    ('x', "")  ==> Error P.(unexpected_end ~expected:'x' ());
  ]

let test_satisfy () =
  let (==>) = test (P.satisfy Char.Ascii.is_upper) in
  T.group "Parser.satisfy is_upper" [
    "A" ==> Ok 'A';
    ""  ==> Error P.(unexpected_end ());
    "0" ==> Error P.(unexpected_token '0');
    "a" ==> Error P.(unexpected_token 'a');
  ]

let test_any () =
  let (==>) = test P.any in
  T.group "Parser.any" [
    "x" ==> Ok 'x';
    "0" ==> Ok '0';
    "?" ==> Ok '?';
    ""  ==> Error P.(unexpected_end ());
  ]

let test_from () =
  let (==>) (options, input) = test (P.from options) input in
  T.group "Parser.from" [
    ([], "x")         ==> Error P.(unexpected_token 'x');
    (['x'], "x")      ==> Ok 'x';
    (['x'; 'y'], "y") ==> Ok 'y';
    (['x'; 'y'], "z") ==> Error P.(unexpected_token 'z');
    ([],  "")         ==> Error P.(unexpected_end ());
  ]

let () = begin
  test_error ();
  test_expect ();
  test_exactly ();
  test_satisfy ();
  test_any ();
  test_from ();
end

