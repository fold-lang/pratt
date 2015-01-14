
open Foundation

module Expression_Parser = Parser.Make(Expression)
open Expression_Parser


let nud_provider : Token.t -> 'a nud option = function
    | Token.Integer x -> Some (atomic (Expression.integer x))
    | Token.Symbol ("-" as op) | Token.Symbol ("+" as op) ->
        Some (prefix Precedence.prefix (fun x -> (Expression.Application ([op], [x]))))
    | Token.Symbol x -> Some (atomic (Expression.variable x))
    | Token.Start -> Some (prefix Precedence.start Expression.start)
    | Token.End -> Some (atomic Expression.fim)


let led_provider : Token.t -> 'a led option = function
    | Token.Symbol op -> Some (infix (Precedence.operator op)
                              (fun a b -> Expression.Application ([op], [a; b])))
    | Token.End       -> Some (postfix Precedence.fim return)
    | x               -> None


let expr_env = function
    | "a" -> 1 | "b" -> 2 | "c" -> 3 | "d" -> 4
    | x -> error ("Unknown variable: `" ^ x ^ "`")


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


open Expression

let () =
    List.iter (curry test) [
        "",         (Start End);
        "1",        (Start (Integer 1));
        "a",        (Start (Variable "a"));
        "2 + 2",    (Start (Application (["+"], [(Integer 2); (Integer 2)])));
        "a = 3",    (Start (Application (["="], [(Variable "a"); (Integer 3)])));
        "a; b",     (Start (Application ([";"], [(Variable "a"); (Variable "b")])));
    ];

    prompt "+"
