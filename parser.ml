
open Foundation

type 'a t = 'a state -> ('a * 'a state)

and 'a state = {
  tokens: Token.t list;
  grammar: Token.t -> 'a handler
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

let ( >>| ) m f = m >>= fun x -> return x

let ( >> ) m x = m >>= fun _ -> x

let run m = fun s -> (m s)

let get = fun s -> (s, s)

let put s = fun _ -> ((), s)


let advance : 'a t =
  get >>= fun state ->
    put {state with tokens = List.tl state.tokens}


let rec parse_loop : int -> 'a t -> 'a t = fun rbp left ->
  get >>= fun {tokens; grammar} ->
    let token = List.hd tokens in
    let lbp, (infix : 'a -> 'a t) =
      match grammar token with
      | `Infix (lbp, infix) -> lbp, infix
      | `Prefix _ -> failwith "Expected an infix handler." in
    if (lbp > rbp)
    then
      left >>= fun expr ->
        let right = infix expr in
        parse_loop rbp right

      (* advance >> infix left >>= \ right -> expression' rbp right *)
      (* advance >> left >>= fun expr ->
        let right = infix expr token in
        parse_loop rbp right *)
    else
      left

(* 
if rbp < lbp s
then do
  advance
  right <- led s left
  expression' rbp right
else
  return left

if rbp < lbp s
then do
  advance >> led s left >>= \right ->
    expression' rbp right
else
  return left
 *)



let parse_expression : int -> 'a t = fun rbp ->
  get >>= fun {tokens; grammar} ->
    let (token : Token.t) = List.hd tokens in
    let (prefix : 'a t) =
      match grammar token with
      | `Prefix prefix -> prefix
      | `Infix _ -> failwith "Expected a prefix handler." in
    let (left : 'a t) = prefix in
    parse_loop rbp left
    (* advance >> parse_loop rbp left *)
    
