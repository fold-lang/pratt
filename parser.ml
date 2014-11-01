
open Foundation


type expr =
    | Add of expr * expr
    | Mul of expr * expr
    | Int of int


let rec string_of_expr expr = match expr with
  | Add (e1, e2) -> Printf.sprintf "(%s + %s)" (string_of_expr e1)
                                               (string_of_expr e2)
  | Mul (e1, e2) -> Printf.sprintf "(%s * %s)" (string_of_expr e1)
                                               (string_of_expr e2)
  | Int n -> string_of_int n


let peek_expr e = print (string_of_expr e); e


type 'a t = state -> ('a * state)

and state =
    {token_stream : Lexing.lexbuf;
          grammar : Token.t -> expr handler;
            token : Token.t}

and 'a handler =
    [ `Prefix of 'a t
    |  `Infix of (int * ('a -> 'a t))]


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
      let lbp, infix = match grammar token with
        | `Infix h -> h
        | `Prefix _ -> failwith "Expected an infix handler." in
      if (lbp > rbp) then
        advance >> infix left >>= fun new_left ->
          parse_loop rbp new_left
      else
        return left


let parse_expression : int -> 'a t = fun rbp ->
  get >>= fun {token; grammar} ->
    let prefix = match grammar token with
      | `Prefix h -> h
      | `Infix _ -> failwith "Expected a prefix handler." in
    prefix >>= fun left ->
      advance >> parse_loop rbp left


