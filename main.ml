
open Foundation

module Expression_Parser = Parser.Make(Expression)
open Expression_Parser

let grammar token =
    let open Expression in
    match token with
    | Token.Letter x ->
        {tok = token;
         lbp = 0;
         led = None;
         nud = Some (return (Atom x))}
    | Token.Symbol "-" ->
        {tok = token;
         lbp = 5;
         nud = Some (return (Atom "-"));
         led = Some (fun a -> parse_expression 5 >>=
                     fun b -> return (List [Atom "-"; a; b]))}
    | Token.Symbol ("--" as post) ->
        {tok = token;
         lbp = 5;
         nud = None;
         led = Some (fun a -> return (List [Atom post; a]))}
    | Token.Symbol x ->
        {tok = token;
         lbp = 5;
         nud = None;
         led = Some (fun a -> parse_expression 5 >>=
                     fun b -> return (List [Atom x; a; b]))}
    | Token.End ->
        {tok = token;
         lbp = 0;
         nud = Some (return (Atom "haha"));
         led = Some return}


let parse (input : string) : Expression.t =
    parse ~lexbuf:(Lexing.from_string input) ~grammar

let (~>) s =
    let e = parse s in
    print ("-> " ^ s);
  print (" = " ^ Expression.show e)

let (==) s e =
    let r = parse s in
    let y = r = e in
    let i = if y then (bright_green "✓ ") else (bright_red "✗ ") in
    print_endline (format "%s %s %s" (bright_blue "->") (bright_white s) i);
    print_endline (format " = %s %s %s" (Expression.show e) "::" (red "Expression"));
    if not y then
        (print_endline (format "\n  Expected: %s" (Expression.show e));
       print_endline (format "    Actual: %s\n" (Expression.show r)))
    else
        print_endline ""


let () =
    let open Expression in
    "a"        == Atom "a";
    "f a"      == List [Atom "f"; Atom "a"];
    "a * b"    == List [Atom "*"; Atom "a"; Atom "b"];
    "f a * b"  == List [Atom "*"; List [Atom "f"; Atom "a"]; Atom "b"];
    "a * f b"  == List [Atom "*"; Atom "a"; List [Atom "f"; Atom "b"]];
    "f a b c"  == List [Atom "f"; Atom "a"; Atom "b"; Atom "c"];
    "-a"       == List [Atom "-"; Atom "a"];
    "a --"     == List [Atom "--"; Atom "a"];
    "-a b + c" == List [Atom "+"; List [Atom "-"; Atom "a"; Atom "b"]; Atom "c"];

    ~> "f x y + - g a - b";
    ~> "- f a b - c --";
    ~> "- - -" (* FIXME: (-) must require at least 1 arg. *)


