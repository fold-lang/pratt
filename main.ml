
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


let parse_int nstr =
  return (Int (int_of_string nstr))


let parse_symbol = function
  | "+" -> (50, fun l -> parse_expression 50 >>| fun r -> Add (l, r))
  | "*" -> (60, fun l -> parse_expression 60 >>| fun r -> Mul (l, r))
  | _ -> failwith "Unknown symbol."


let grammar : Token.t -> expr Parser.handler =
  Token.(function
  | { kind = Number; text } -> `Prefix (parse_int text)
  | { kind = Symbol; text } ->  `Infix (parse_symbol text)
  | _ -> failwith "Unknown token.")


let expr_lexbuf = Lexing.from_string "2 + 1 * 3"


let _ =
  let state =
    let next () = Lexer.token expr_lexbuf in
    {tokens = repeat_until next Token.eof; grammar} in

    Token.print_list @@ state.tokens;

  Parser.run (Parser.parse_expression 0) state
