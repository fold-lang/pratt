
open Foundation

module Expression_Parser = Parser.Make(Expression)
open Expression_Parser

(* error (format "no led for token %s" (Token.show t)) *)

let nud_provider : Token.t -> 'a nud option = function
  | Token.Letter x -> Some (function | (Expression.Nud nud) -> return (Expression.Nud (nud @ [x]))
                     | _ -> error "needs a nud")
  | _ -> None

let led_provider : Token.t -> 'a led option = function
  | Token.Symbol x -> Some (5, fun l -> parse_expression 5 >>=
                 fun r -> return (Expression.Led (x, l, r)))
  | Token.End -> Some (0, return)
  | _ -> None

let parse (s : string) : Expression.t =
  let lexbuf = Lexing.from_string s in
  let grammar = { led_provider; nud_provider } in
  parse ~lexbuf ~grammar

let (~>) s =
  let e = parse s in
  print ("-> " ^ s);
  print (" = " ^ Expression.show e)

let (=>) s e =
  let icon = if parse s = e
               then (green "✓ ")
               else (red "✗ ")
  in
    print_endline (format " %s %s" (bright_blue "->") (bright_white s));
    print_endline (format "  %s %s %s\n" (yellow "=") (Expression.show e) icon)


open Expression
let () =
  "a"       => Nud ["a"];
  "a b"     => Nud ["a"; "b"];
  "a * b"   => Led ("*", Nud ["a"], Nud ["b"]);
  "f a * b" => Led ("*", Nud ["f"; "a"], Nud ["b"])
