
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
         led = Some (fun e1 -> parse_expression 5 >>=
                     fun e2 -> return (List [Atom "-"; e1; e2]))}
    | Token.Symbol a ->
        {tok = token;
         lbp = 5;
         nud = None;
         led = Some (fun e1 -> parse_expression 5 >>=
                     fun e2 -> return (List [Atom a; e1; e2]))}
    | Token.End ->
        {tok = token;
         lbp = 0;
         nud = None;
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
    let i = if y then (green "✓ ") else (red "✗ ") in
    print_endline (format "%s %s %s" (bright_blue "->") (bright_white s) i);
    print_endline (format " = %s %s %s" (Expression.show e) "::" (bright_red "Expression"));
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
    "-a b + c" == List [Atom "+"; List [Atom "-"; Atom "a"; Atom "b"]; Atom "c"];

    ~> "f x y + - g a - b";
    ~> "- - -" (* FIXME: (-) must require at least 1 arg. *)


