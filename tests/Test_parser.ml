open Astring

module Stream = Pratt.Stream
module P = Pratt.Make(Char)
module T = Nanotest


(* Helper test function, tests a particular [parser] with a given [input]. *)
let test parser input expected =
  let printer = T.(result char (testable P.pp_error)) in
  let actual  = Result.map _1 @@ P.run parser (Stream.of_string input) in
  let message = "input: \"" ^ input ^ "\"" in
  T.test ~verbose:true printer message ~expected ~actual

(* Helper test function, tests a particular [parser] with a given [input]. *)
let test' parser input expected =
  let printer = T.result T.(list char) (T.testable P.pp_error) in
  let actual  = Result.map _1 @@ P.run parser (Pratt.Stream.of_string input) in
  let message = "input: \"" ^ input ^ "\"" in
  T.test ~verbose:true printer message ~expected ~actual


let test_error () =
  let (==>) = test P.(error Zero) in
  T.group "Parser.error" [
    ""    ==> Error P.Zero;
    "a"   ==> Error P.Zero;
    "abc" ==> Error P.Zero;
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

let test_while () =
  let (==>) input = test' P.(many_while ((!=) 'x') any) input in
  T.group "Parser.many_while" [
    ""         ==> Ok [];
    "x"        ==> Ok [];
    "ax"       ==> Ok ['a'];
    "abcx"     ==> Ok ['a'; 'b'; 'c'];
    "abcxxx"   ==> Ok ['a'; 'b'; 'c'];
    "abc"      ==> Ok ['a'; 'b'; 'c'];
  ]

let test_many () =
  let (==>) input = test' P.(many any) input in
  T.group "Parser.many_while" [
    ""         ==> Ok [];
    "ax"       ==> Ok ['a'];
    "abc"      ==> Ok ['a'; 'b'; 'c'];
  ]

let test_many_while () =
  let (==>) input = test' P.(many_while ((!=) 'x') any) input in
  T.group "Parser.many_while" [
    ""         ==> Ok [];
    "ax"       ==> Ok ['a'];
    "abcx"     ==> Ok ['a'; 'b'; 'c'];
    "abc"      ==> Ok ['a'; 'b'; 'c'];
  ]

let () = begin
  test_error ();
  test_expect ();
  test_exactly ();
  test_satisfy ();
  test_any ();
  test_from ();
  test_many ();
  test_many_while ();
end

