
type 'a t = unit -> 'a s
and 'a s = Yield of 'a * 'a t | Empty

let rec count n () = Yield (n, count (n + 1))

let rec make n f =
  let rec loop i () =
    if i = n then Empty
    else Yield (f i, loop (i + 1)) in
  loop 0

let rec fold f acc stream =
  match stream () with
  | Empty -> acc
  | Yield (x, k) -> fold f (f acc x) k

let rec map f stream () =
  match stream () with
  | Empty -> Empty
  | Yield (x, k) -> Yield (f x, map f k)

let rec take n stream () =
  match stream () with
  | Empty -> Empty
  | Yield (x, k) ->
    if n = 0 then Empty
    else Yield (x, (take (n - 1) k))

let rec head stream =
  match stream () with
  | Empty -> None
  | Yield (x, _) -> Some x

let is_empty (stream : 'a t) =
  match stream () with
  | Empty -> true
  | Yield _ -> false

let rec of_list l () =
  match l with
  | [] -> Empty
  | x :: xs -> Yield (x, of_list xs)

let to_list stream =
  let rec loop acc stream' =
    match stream' () with
    | Empty -> acc
    | Yield (x, k) -> loop (List.cons x acc) k in
  List.rev (loop [] stream)

let of_string str =
  let rec loop i () =
    try
      Yield (String.get str i, loop (i + 1))
    with Invalid_argument _ ->
      Empty in
  loop 0

let rec filter p stream () =
  match stream () with
  | Empty -> Empty
  | Yield (x, k) ->
    if p x then
      Yield (x, filter p k)
    else
      filter p k ()

let reject p stream =
  filter (fun x -> not (p x)) stream

let next stream =
  match stream () with
  | Empty -> None
  | Yield (x, stream') -> Some (x, stream')

