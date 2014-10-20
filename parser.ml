
open Foundation


type expr =
    | Add of expr * expr
    | Mul of expr * expr
    | Int of int



let rec string_of_expr expr = match expr with
  | Add (e1, e2) -> Printf.sprintf "(+ %s %s)" (string_of_expr e1) (string_of_expr e1)
  | Mul (e1, e2) -> Printf.sprintf "(* %s %s)" (string_of_expr e1) (string_of_expr e1)
  | Int n -> string_of_int n



type 'a t = state -> ('a * state)

and state = {
  tokens: Token.t list;
  grammar: Token.t -> expr handler
}

and 'a handler =
    [ `Prefix of 'a t
    |  `Infix of (int * ('a -> 'a t))]


let return
  : 'a -> 'a t
  = fun x -> fun s -> (x, s)

let bind m f = fun s ->
  let (x, s') = m s in
  f x s'

let ( >>= ) = bind

let ( >>| ) m f = m >>= fun x -> return (f x)

let ( >> ) m x = m >>= fun _ -> x

let run m = fun s -> (m s)

let get = fun s -> (s, s)

let put s = fun _ -> ((), s)


let advance : 'a t =
  get >>= fun state ->
    put {state with tokens = List.tl state.tokens}


let rec parse_loop : int -> 'a -> 'a t = fun rbp left ->
  get >>= fun {tokens; grammar} ->
    if (List.length tokens = 0) then
      return left
    else

    let token = List.hd tokens in
    let lbp, infix = match grammar token with
      | `Infix (lbp, infix) -> lbp, infix
      | `Prefix _ -> failwith "Expected an infix handler." in
    (Printf.printf "lbp(%d) > rbp(%d)\n" lbp rbp);
    if (lbp > rbp) then
      advance >> infix left >>= fun right ->
        parse_loop rbp right
    else
      return left


let parse_expression : int -> 'a t = fun rbp ->
  get >>= fun {tokens; grammar} ->
    let token = List.hd tokens in
    let prefix = match grammar token with
      | `Prefix prefix -> prefix
      | `Infix _ -> failwith "Expected a prefix handler." in
    prefix >>= fun left -> advance >> parse_loop rbp left
