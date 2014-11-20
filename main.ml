
open Foundation

module Expression_Parser = Parser.Make(Expression)
open Expression_Parser

let infix_operator_parselet = function
    | "*" -> infix Precedence.multiplication Expression.multiplication
    | "+" -> infix Precedence.addition       Expression.addition
    | "-" -> infix Precedence.subtraction    Expression.subtraction
    | "=" -> infix Precedence.assignment     Expression.assignment
    | ";" -> infix Precedence.sequence       Expression.sequence
    | x   -> error (format "Unsuported operator symbol: `%s`." x)

let get_prefix_handler : Token.t -> 'a prefix_handler = function
    | Token.Integer x  -> atomic (Expression.integer x)
    | Token.Symbol "-" -> prefix Precedence.prefix Expression.negation
    | Token.Symbol "+" -> prefix Precedence.prefix identity
    | Token.Symbol  x  -> atomic (Expression.variable x)
    | Token.Start      -> prefix Precedence.start Expression.start
    | Token.End        -> atomic Expression.fim

let get_infix_handler : Token.t -> 'a infix_handler = function
    | Token.Symbol x -> infix_operator_parselet x
    | Token.End      -> postfix Precedence.fim return
    | x              -> error (format "Expected infix token type, got: `%s`"
                                      (Token.show x))

let expr_env = function
    | "a" -> 1 | "b" -> 2 | "c" -> 3 | "d" -> 4
    | x -> error ("Unknown variable: `" ^ x ^ "`")


let input = "a = 5; 2 + 2"
let lexbuf = Lexing.from_string input
let grammar = {
    infix = get_infix_handler;
    prefix = get_prefix_handler
}

let welcome_msg = "--\n-- Metaphor\n--"

let () =
    print welcome_msg;
    let result = parse ~lexbuf ~grammar in
    print ("-> " ^ input);
    print (" = " ^ Expression.show result)


