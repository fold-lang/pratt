
type 'a t = Yield of 'a * (unit -> 'a t) | Empty

let rec count n : int t = Yield (n, fun () -> count (n + 1))

let rec make n f : 'a t =
  let rec loop i =
    if i = n then Empty
    else Yield (f i, fun () -> loop (i + 1)) in
  loop 0

let rec fold f acc (stream : 'a t) =
  match stream with
  | Empty -> acc
  | Yield (x, k) -> fold f (f acc x) (k ())

let rec map f (stream : 'a t) =
  match stream with
  | Empty -> Empty
  | Yield (x, k) -> Yield (f x, fun () -> map f (k ()))

let rec head stream =
  match stream with
  | Empty -> None
  | Yield (x, _) -> Some x

let is_empty (stream : 'a t) =
  match stream with
  | Empty -> true
  | _ -> false

let rec of_list list : 'a t =
  match list with
  | [] -> Empty
  | x :: xs -> Yield (x, fun () -> of_list xs)

let to_list (stream : 'a t) =
  let rec loop acc stream' =
    match stream' with
    | Empty -> acc
    | Yield (x, k) -> loop (List.cons x acc) (k ()) in
  List.rev (loop [] stream)

let of_string str : char t =
  let rec loop i =
    try
      Yield (String.get str i, fun () -> loop (i + 1))
    with Invalid_argument _ ->
      Empty in
  loop 0

let rec filter p stream  =
  match stream with
  | Empty -> Empty
  | Yield (x, k) ->
    if p x then
      Yield (x, fun () -> filter p (k ()))
    else
      filter p (k ())

let reject p stream =
  filter (fun x -> not (p x)) stream

let next stream =
  match stream with
  | Empty -> None
  | Yield (x, stream') -> Some (x, stream' ())

