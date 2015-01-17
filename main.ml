
open Foundation

module Expression_Parser = Parser.Make(Expression)
open Expression_Parser

let minus_symbol tok =
    let expr_sym = Expression.Sym "-" in
    {tok = tok;
     lbp = 5;
     nud = Some (fun e1 -> return (Expression.append e1 expr_sym));
     led = Some (fun e1 -> parse_expression 5 >>=
         fun e2 -> return (Expression.Led (expr_sym, e1, e2)))}

let grammar token = match token with
  | Token.Letter x ->
          {tok = token;
     lbp = 0;
     led = None;
     nud = Some (fun e1 -> return (Expression.append e1 (Expression.Sym x)))}
  | (Token.Symbol "-") as tok -> minus_symbol tok
  | Token.Symbol x ->
          {tok = token;
     lbp = 5;
     nud = None;
     led = Some (fun l -> parse_expression 5 >>=
         fun r -> return (Expression.Led (Expression.Sym(x), l, r)))}
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

let (=>) s e =
    let r = parse s in
    let y = r = e in
    let i = if y then (cyan "✓ ") else (red "✗ ") in
    print_endline (format "%s %s" (bright_blue "->") (bright_white s));
    print_endline (format ":: %s %s %s %s" (bright_red "Expression") "=" (Expression.show e) i);
    if not y then
        (print_endline (format "\n  Expected: %s" (Expression.show e));
       print_endline (format "    Actual: %s\n" (Expression.show r)))
    else
        print_endline ""


let () =
    let open Expression in
    "a"        => Nud [Sym "a"];
    "a b"      => Nud [Sym "a"; Sym "b"];
    "a * b"    => Led (Sym "*", Nud [Sym "a"], Nud [Sym "b"]);
    "f a * b"  => Led (Sym "*", Nud [Sym "f"; Sym "a"], Nud [Sym "b"]);
    "a * f b"  => Led (Sym "*", Nud [Sym "a"], Nud [Sym "f"; Sym "b"]);
    "f a b c"  => Nud [Sym "f"; Sym "a"; Sym "b"; Sym "c"];
    "-a"       => Nud [Sym "-"; Sym "a"];
    "-a b + c" => Led (Sym "+", Nud [Sym "-"; Sym "a"; Sym "b"], Nud [Sym "c"]);

