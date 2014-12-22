
open Foundation

module Expression_Parser = Parser.Make(Expression)
open Expression_Parser

let nud_provider : Token.t -> 'a nud = Expression.(function
    | Token.Integer x -> atomic (Expression.integer x)
    | Token.Symbol ("-" as op) | Token.Symbol ("+" as op) ->
        prefix Precedence.prefix (fun x -> (Application ([op], [x])))
    | Token.Symbol  x  -> atomic (Expression.variable x)
    | Token.Start      -> prefix Precedence.start Expression.start
    | Token.End        -> atomic Expression.fim)

let led_provider : Token.t -> 'a led = Expression.(function
    | Token.Symbol op -> infix (Precedence.operator op)
                               (fun a b -> Application ([op], [a; b]))
    | Token.End       -> postfix Precedence.fim return
    | x               -> error (format "Expected infix token type, got: `%s`"
                                      (Token.show x)))

let expr_env = function
    | "a" -> 1 | "b" -> 2 | "c" -> 3 | "d" -> 4
    | x -> error ("Unknown variable: `" ^ x ^ "`")


let exec input = 
    let lexbuf = Lexing.from_string input in
    let result = parse ~lexbuf ~grammar: { led_provider; nud_provider } in
    print ("-> " ^ input);
    print (" = " ^ Expression.show result)


let () =
    exec "1";
    exec "a";
    exec "a = 3";
    exec "2 + 3";
    exec "2 + 3; a";
    exec "";


