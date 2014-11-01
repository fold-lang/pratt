
open Foundation
open Parser

let rec eval = function
    | Int a -> a
    (* | Var a -> a *)
    | Add (a, b) -> (eval a) + (eval b)
    | Mul (a, b) -> (eval a) * (eval b)


let parse_int = prefix (fun x -> Int (int_of_string x))
(* let parse_var = prefix (fun x -> Var var_str) *)

let parse_symbol = function
  | "+" -> infix 50 (fun a b -> Add (a, b))
  | "*" -> infix 60 (fun a b -> Mul (a, b))
  | x -> failwith ("Unknown symbol: `" ^ x ^ "`.")


let grammar : Token.t -> expr Parser.handler =
  Token.(function
  | Number x -> parse_int x
  | Symbol x -> parse_symbol x
  | _ -> failwith ("Unknown token."))


let expr_str = "2 + 1 * 3 + 4 + 1 * 2 * 3"
let expr_lexbuf = Lexing.from_string expr_str

let () =
  let state = init_state expr_lexbuf grammar in
  let expr = first (run (parse_expression 0) state) in
  print_endline @@ "Input:  " ^ expr_str;
  print_endline @@ "Output: " ^ string_of_expr expr;
  print_endline @@ "Result: " ^ string_of_int (eval expr)


