
open Foundation

module Expression_Parser = Parser.Make(Expression)
open Expression_Parser


let nud_provider : Token.t -> 'a nud option = function
    | Token.Start -> Some (prefix Precedence.start Expression.start)
    | Token.End   -> Some (atomic Expression.fim)

let led_provider : Token.t -> 'a led option = function
    | Token.End -> Some (postfix Precedence.fim return)
    | _         -> None

let parse s =
    let lexbuf = Lexing.from_string s in
    let grammar = { led_provider; nud_provider } in
    parse ~lexbuf ~grammar

let prompt s =
    let e = parse s in
    print ("-> " ^ s);
    print (" = " ^ Expression.show e)

let test s e =
    assert (parse s = e)

let () =
    prompt ""
