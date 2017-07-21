
module P = Pratt.Parser.String
module T = Nanotest


let test_parser p input expected =
  T.(test (result char (T.of_pp (P.pp_error Fmt.char))))
    (if input = "" then "<empty>" else input)
    expected (P.parse p input)


let () = begin
  let (==>) (x, input) = test_parser (P.exactly x) input in
  T.group "Parser.exactly" [
    ('x', "x") ==> Ok 'x';
    ('x', "y") ==> Error P.(Unexpected_token { expected = 'x'; actual = 'y' });
    ('x', "")  ==> Error P.(Unexpected_end   { expected = 'x' });
  ];

  let (==>) = test_parser P.any in
  T.group "Parser.any" [
    "x" ==> Ok 'x';
    "0" ==> Ok '0';
    "?" ==> Ok '?';
    ""  ==> Error Empty;
  ];

  let (==>) (options, input) = test_parser (P.one_of options) input in
  T.group "Parser.one_of" [
    ([], "x")         ==> Error P.(Failed_satisfy 'x');
    (['x'], "x")      ==> Ok 'x';
    (['x'; 'y'], "y") ==> Ok 'y';
    (['x'; 'y'], "z") ==> Error P.(Failed_satisfy 'z');
    ([],  "")         ==> Error Empty;
  ];
end

