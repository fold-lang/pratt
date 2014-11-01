
open Foundation
open Parser


let parse_int int_str = return (Int (int_of_string int_str))

let parse_symbol = function
  | "+" -> (50, fun l -> parse_expression 50 >>| fun r -> Add (l, r))
  | "*" -> (60, fun l -> parse_expression 60 >>| fun r -> Mul (l, r))
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
  print_endline @@ "Output: " ^ string_of_expr expr


