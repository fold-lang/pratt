
open Foundation

module Expression_Parser = Parser.Make(Expression)
open Expression_Parser


let nud_provider : Token.t -> 'a nud option = function
    | Token.Letter x -> Some (return (Expression.Nud [x]))
    | t -> error (format "no nud for token %s" (Token.show t))

let led_provider : Token.t -> 'a led option = function
    | Token.Symbol x -> Some (5, fun l -> parse_expression 5 >>=
                                 fun r -> return (Expression.Led (x, l, r)))
    | Token.End -> Some (0, return)
    | t -> error (format "no led for token %s" (Token.show t))

let parse (s : string) : Expression.t =
    let lexbuf = Lexing.from_string s in
    let grammar = { led_provider; nud_provider } in
    parse ~lexbuf ~grammar

let (~>) s =
    let e = parse s in
    print ("-> " ^ s);
    print (" = " ^ Expression.show e)

let test s e =
    assert (parse s = e)

let () =
    ~> "a * b"
