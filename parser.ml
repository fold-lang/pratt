
open Foundation

type 'a t = 'a state -> ('a * 'a state)

and 'a state = {
  tokens: Token.t list;
  grammar: Token.t -> 'a handler
}

and 'a handler =
    [ `Prefix of (Token.t -> 'a t)
    | `Infix of (int * ('a -> Token.t -> 'a t))]


let return
  : 'a -> 'a t
  = fun x -> fun s -> (x, s)

let bind m f = fun s ->
  let (x, s') = m s in
  f x s'

let ( >>= ) = bind

let ( >> ) u v = u >>= fun _ -> v

let run m = fun s -> (m s)

let get = fun s -> (s, s)

let put s = fun _ -> ((), s)


let advance : 'a t =
  get >>= fun {tokens; grammar} ->
    put {tokens = List.tl tokens; grammar}


let rec parse_loop : int -> 'a t -> 'a t = fun rbp left ->
  get >>= fun {tokens; grammar} ->
    let token = List.hd tokens in
    let lbp, (infix : 'a -> Token.t -> 'a t) =
      match grammar token with
      | `Infix (lbp, infix) -> lbp, infix
      | `Prefix _ -> failwith "Expected an infix handler." in
    if (lbp > rbp)
    then
      left >>= fun expr ->
        let right = infix expr token in
          parse_loop rbp right

(*       advance >> left >>= fun expr ->
        let right = infix expr token in
          parse_loop rbp right *)
    else
      left


let parse_expression : int -> 'a t = fun rbp ->
  get >>= fun {tokens; grammar} ->
    let (token : Token.t) = List.hd tokens in
    let (prefix : Token.t -> 'a t) =
      match grammar token with
      | `Prefix prefix -> prefix
      | `Infix _ -> failwith "Expected a prefix handler." in
    let (left : 'a t) = prefix token in
    parse_loop rbp left
    (* advance >> parse_loop rbp left *)
    
