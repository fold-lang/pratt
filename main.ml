
open Foundation

module Expression_Parser = Parser.Make(Expression)
open Expression_Parser


let grammar token = match token with
  | Token.Letter x ->
    {tok = token;
     lbp = 0;
     led = None;
     nud = Some (function | (Expression.Nud nud) -> return (Expression.Nud (nud @ [x]))
                          | _ -> error "needs a nud")}
  | Token.Symbol x ->
    {tok = token;
     lbp = 5;
     nud = None;
     led = Some (fun l -> parse_expression 5 >>=
                 fun r -> return (Expression.Led (x, l, r)))}
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
  let icon = if parse s = e
               then (cyan "✓ ")
               else (red "✗ ") in
    print_endline (format "%s %s" (bright_blue "->") (bright_white s));
    print_endline (format ":: %s %s %s %s" (bright_red "Expression") "=" (Expression.show e) icon);
    print_endline ""


open Expression
let () =
  "a"       => Nud ["a"];
  "a b"     => Nud ["a"; "b"];
  "a * b"   => Led ("*", Nud ["a"], Nud ["b"]);
  "f a * b" => Led ("*", Nud ["f"; "a"], Nud ["b"])

