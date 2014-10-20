
type expr =
    | Add of expr * expr
    | Mul of expr * expr
    | Int of int

(* expr Parser.t = expr Parser.state -> expr * expr Parser.state *)

(* let plus_handler : expr -> Token.t -> expr Parser.t = Parser.(
  fun left -> (parse_expression 50 >>= fun right -> return (Add (left, right)))
)
 *)

open Parser
open Foundation


let int_handler token =
  return (Int (int_of_string Token.(token.text)))

let add_handler expr token =
  parse_expression 50 >>= fun right -> return (Add (expr, right))

let mul_handler expr token =
  parse_expression 60 >>= fun right -> return (Mul (expr, right))


let grammar : Token.t -> expr Parser.handler =
  Token.(function
  | { kind = Number } -> `Prefix int_handler
  | { kind = Symbol; text = "+" } -> `Infix (50, add_handler)
  | { kind = Symbol; text = "*" } -> `Infix (60, mul_handler)
  | _ -> failwith "Unknown token.")


let expr_lexbuf = Lexing.from_string "2 + 1 * 3"


let _ =
  let state =
    let next () = Lexer.token expr_lexbuf in
    {tokens = repeat_until next Token.eof; grammar} in

    Token.print_list @@ state.tokens;

  Parser.run (Parser.parse_expression 0) state
