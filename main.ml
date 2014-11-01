
open Foundation
open Parser

let rec eval = function
    | Int a -> a
    | Add (a, b) -> (eval a) + (eval b)
    | Mul (a, b) -> (eval a) * (eval b)


let parse_int int_str = return (Int (int_of_string int_str))

let parse_symbol = function
  | "+" -> infix 50 (fun l r -> Add (l, r))
  | "*" -> infix 60 (fun l r -> Mul (l, r))
  | x -> failwith ("Unknown symbol: `" ^ x ^ "`.")

let grammar : Token.t -> expr Parser.handler =
  Token.(function
  | { kind = Number; text } -> `Prefix (parse_int text)
  | { kind = Symbol; text } ->  `Infix (parse_symbol text)
  | _ -> failwith ("Unknown token."))


let expr_str = "2 + 1 * 3 + 4 + 1 * 2 * 3"
let expr_lexbuf = Lexing.from_string expr_str

let () =
  let state = init_state expr_lexbuf grammar in
  let expr = first (run (parse_expression 0) state) in
  print_endline @@ "Input:  " ^ expr_str;
  print_endline @@ "Output: " ^ string_of_expr expr;
  print_endline @@ "Result: " ^ string_of_int (eval expr)


