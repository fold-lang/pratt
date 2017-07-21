
module P = Pratt.Parser.String
module T = Nanotest


(* Helper test function, tests a particular [parser] with a given [input]. *)
let test parser input expected =
  let message = "input: " ^ if input = "" then "<empty>" else input in
  let printer = T.result T.char (T.of_pp (P.pp_error Fmt.char)) in
  let actual  = P.parse parser input in
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
    ""    ==> Error P.(Unexpected_end   { expected = 'a' });
    "x"   ==> Error P.(Unexpected_token { expected = 'a'; actual = 'x' });
    "a"   ==> Ok 'a';
    "abc" ==> Ok 'a';
  ]

let test_exactly () =
  let (==>) (x, input) = test (P.exactly x) input in
  T.group "Parser.exactly" [
    ('x', "x") ==> Ok 'x';
    ('x', "y") ==> Error P.(Unexpected_token { expected = 'x'; actual = 'y' });
    ('x', "")  ==> Error P.(Unexpected_end   { expected = 'x' });
  ]

let test_satisfy () =
  let (==>) = test (P.satisfy Char.Ascii.is_upper) in
  T.group "Parser.satisfy is_upper" [
    "A" ==> Ok 'A';
    ""  ==> Error P.Empty;
    "0" ==> Error (P.Failed_satisfy '0');
    "a" ==> Error (P.Failed_satisfy 'a');
  ]

let test_any () =
  let (==>) = test P.any in
  T.group "Parser.any" [
    "x" ==> Ok 'x';
    "0" ==> Ok '0';
    "?" ==> Ok '?';
    ""  ==> Error Empty;
  ]

let test_one_of () =
  let (==>) (options, input) = test (P.one_of options) input in
  T.group "Parser.one_of" [
    ([], "x")         ==> Error P.(Failed_satisfy 'x');
    (['x'], "x")      ==> Ok 'x';
    (['x'; 'y'], "y") ==> Ok 'y';
    (['x'; 'y'], "z") ==> Error P.(Failed_satisfy 'z');
    ([],  "")         ==> Error Empty;
  ]

let () = begin
  test_error ();
  test_expect ();
  test_exactly ();
  test_satisfy ();
  test_any ();
  test_one_of ();
end

