
open Foundation

type state =
    {token_stream : Lexing.lexbuf;
          grammar : Expression.t grammar;
            token : Token.t}

and 'a t = state -> ('a * state)

and 'a handler =
    [ `Prefix of 'a prefix_handler
    |  `Infix of 'a infix_handler]

and 'a prefix_handler = 'a t

and 'a infix_handler = int * ('a -> 'a t)


and 'a grammar = {
   infix : Token.t -> 'a infix_handler;
  prefix : Token.t -> 'a prefix_handler
}


(* State Monad *)

let return : 'a -> 'a t =
  fun x -> fun s -> (x, s)

let bind m f =
  fun s -> let (x, s') = m s in f x s'

let ( >>= ) = bind

let ( >>| ) m f = m >>= fun x -> return (f x)

let ( >> ) m x = m >>= fun _ -> x

let run m = fun s -> (m s)

let get = fun s -> (s, s)

let put s = fun _ -> ((), s)


(* Parsing *)

let init_state token_stream grammar =
  let first_token = Lexer.token token_stream in
  {token_stream; grammar; token = first_token}


let advance : 'a t =
  get >>= fun state ->
    put {state with token = Lexer.token state.token_stream}


let rec parse_loop : int -> 'a -> 'a t = fun rbp left ->
  get >>= fun {token; grammar} ->
    if Token.is_end token then
      return left
    else
      let lbp, infix = grammar.infix token in
      if (lbp > rbp) then
        advance >> infix left >>= fun new_left ->
          parse_loop rbp new_left
      else
        return left


let parse_expression : int -> 'a t = fun rbp ->
  get >>= fun {token; grammar} ->
    let prefix = grammar.prefix token in
    prefix >>= fun left ->
      advance >> parse_loop rbp left



let infix precedence expr_builder =
    (precedence, fun left ->
        parse_expression precedence >>| fun right ->
            expr_builder left right)

let prefix expr_builder =
  fun x -> return (expr_builder x)



