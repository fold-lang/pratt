
open Foundation

module Expression_Parser = Parser.Make(Expression)
open Expression_Parser

let parse_int = prefix (fun x -> Expression.Int (int_of_string x))
let parse_var = prefix (fun x -> Expression.Var x)
let parse_op  = function
  | "+" -> infix 50 (fun a b -> Expression.Add (a, b))
  | "*" -> infix 60 (fun a b -> Expression.Mul (a, b))
  | "-" -> infix 50 (fun a b -> Expression.Sub (a, b))
  | x -> failwith ("Unknown operator symbol: `" ^ x ^ "`.")

let get_prefix_handler : Token.t -> 'a prefix_handler =
  function Token.Number x -> parse_int x
         | Token.Symbol x -> parse_var x
         | x -> failwith ("Unknown token type: `" ^ (Token.to_string x) ^ "`")

let get_infix_handler : Token.t -> 'a infix_handler =
  function Token.Symbol x -> parse_op x
         | x -> failwith ("Unknown token type: `" ^ (Token.to_string x) ^ "`")



let expr_str = "2 + 1 * 3 + a - 4"
let expr_env = function
    | "a" -> 4
    | "b" -> 2
    | x -> failwith ("Unknown variable: `" ^ x ^ "`")

let expr_lexbuf = Lexing.from_string expr_str

let () =
  let grammar = { infix = get_infix_handler;
                 prefix = get_prefix_handler} in
  let state = init_state expr_lexbuf grammar in
  let expr = first (run (parse_expression 0) state) in
  print_endline @@ "Input:  " ^ expr_str;
  print_endline @@ "Output: " ^ Expression.to_string expr;
  print_endline @@ "Result: " ^ string_of_int (Expression.eval expr_env expr)


