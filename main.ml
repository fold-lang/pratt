
open Foundation

module Expression_Parser = Parser.Make(Expression)
open Expression_Parser

let atomic_int_parselet = atomic (fun x -> Expression.Int x)

let atomic_var_parselet = atomic (fun x -> Expression.Var x)

let infix_operator_parselet = function
    | "*" -> infix 0x0040 (fun a b -> Expression.Mul (a, b))
    | "+" -> infix 0x0030 (fun a b -> Expression.Add (a, b))
    | "-" -> infix 0x0030 (fun a b -> Expression.Sub (a, b))
    | "=" -> infix 0x0010 (fun a b -> Expression.Assignment (a, b))
    | x -> failwith ("Unknown operator symbol: `" ^ x ^ "`.")

let prefix_minus_parselet = prefix 0x0060 (fun x -> Expression.Not x)

let get_prefix_handler : Token.t -> 'a prefix_handler = function
    | Token.Integer x -> atomic_int_parselet x
    | Token.Symbol "-" -> prefix_minus_parselet
    | Token.Symbol x -> atomic_var_parselet x
    | Token.Start -> prefix 0x0000 (fun x -> Expression.Start x)
    | Token.End -> return Expression.End

let get_infix_handler : Token.t -> 'a infix_handler = function
    | Token.Symbol x -> infix_operator_parselet x
    | Token.End -> postfix 0x0000 return
    | x -> failwith ("Unknown infix token type: `" ^ (Token.show x) ^ "`")

let expr_env = function
    | "a" -> 1 | "b" -> 2 | "c" -> 3 | "d" -> 4
    | x -> failwith ("Unknown variable: `" ^ x ^ "`")


let input = "2 + -3"
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


