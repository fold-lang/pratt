
open Foundation

module Expression_Parser = Parser.Make(Expression)
open Expression_Parser

let minus_symbol tok =
    let open Expression in
    {tok = tok;
     lbp = 5;
     nud = Some (return (Sym "-"));
     led = Some (fun e1 -> parse_expression 5 >>=
                 fun e2 -> return (Led (Sym "-", e1, e2)))}

let grammar token =
    let open Expression in
    match token with
    | Token.Letter x ->
        {tok = token;
         lbp = 0;
         led = None;
         nud = Some (return (Sym x))}
    | (Token.Symbol "-") as tok -> minus_symbol tok
    | Token.Symbol x ->
        {tok = token;
         lbp = 5;
         nud = None;
         led = Some (fun e1 -> parse_expression 5 >>=
                     fun e2 -> return (Led (Sym x , e1, e2)))}
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
    "a"        == Sym "a";
    "f a"      == Nud [Sym "f"; Sym "a"];
    "a * b"    == Led (Sym "*", Sym "a", Sym "b");
    "f a * b"  == Led (Sym "*", Nud [Sym "f"; Sym "a"], Sym "b");
    "a * f b"  == Led (Sym "*", Sym "a", Nud [Sym "f"; Sym "b"]);
    "f a b c"  == Nud [Sym "f"; Sym "a"; Sym "b"; Sym "c"];
    "-a"       == Nud [Sym "-"; Sym "a"];
    "-a b + c" == Led (Sym "+", Nud [Sym "-"; Sym "a"; Sym "b"], Sym "c");

    ~> "f x y + - g a - b";
    ~> "- - -" (* FIXME: (-) must require at least 1 arg. *)


