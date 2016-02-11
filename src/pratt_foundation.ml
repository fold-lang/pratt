
(* Applies `f` to the arguments `x` and `y` in reveresed orded. *)
let flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c =
  fun f x y -> f y x

(* Mathematical composition of functions: `(f ∘ g)(x) = f(g(x))`. *)
let compose f g = fun x -> f (g x)

let invcompose g f = fun x -> f (g x)

(* Infix function composition operator. *)
let (<<) = compose

(* Infix inverse function composition operator. *)
let (>>) = invcompose

type void = Void

let void _ = ()

(* Identity function. *)
let identity : 'a -> 'a = fun x -> x

let const x = fun _ -> x

(* Ignores the passed argument and returns unit. *)
let ignore : 'a -> unit = fun _ -> ()

let exclusive_option_exn x y =
  match (x, y) with
  | Some v, None -> v
  | None, Some v -> v
  | None, None -> failwith "At least one of two options must be defined."
  | Some _, Some _ -> failwith "Both options are defined."


let defined_option = function
  | Some _ -> true
  | None -> false


let (++) = List.append
let (@) = (@@)


let rec repeat_until fn limit =
  let x = fn () in
  if (x = limit)
    then []
    else [x] ++ (repeat_until fn limit)


let rec repeat_fn_to fn limit =
  let x = fn () in
  if (x = limit)
    then []
    else [x] ++ (repeat_fn_to fn limit)

let rec repeatedly n f =
  if n = 0
    then []
    else (f ()) :: (repeatedly (n - 1) f)

let rec range ?(start=0) ?(step=1) stop =
  if start >= stop
    then []
    else start :: (range ~start: (start + step) stop ~step)

let print = print_endline
let p = print_endline

let printf = Printf.printf

let fmt = Printf.sprintf

let first (x, _) = x
let second (_, y) = y

module type Type = sig
  type t
end

let join = String.concat
let map = List.map


let curry f (x, y) = f x y

type 'a result =
  | Ok of 'a
  | Error of string

let color_format color =
  fmt "\027[%dm%s\027[0m"
     (match color with
      | `Black   -> 30
      | `Red     -> 31
      | `Green   -> 32
      | `Yellow  -> 33
      | `Blue    -> 34
      | `Magenta -> 35
      | `Cyan    -> 36
      | `White   -> 37)

let blue = color_format `Blue
let red = color_format `Red
let yellow = color_format `Yellow
let magenta = color_format `Magenta
let cyan = color_format `Cyan
let white = color_format `White
let green = color_format `Green
let bright_white x = fmt "\027[1;37m%s\027[0m" x
let bright_blue x = fmt "\027[1;34m%s\027[0m" x
let bright_magenta x = fmt "\027[1;35m%s\027[0m" x
let violet x = fmt "\027[0;34m%s\027[0m" x
(* let violet x = format "\027[0;49;34m%s\027[0m" x *)
let bright_red x = fmt "\027[1;31m%s\027[0m" x
let bright_green x = fmt "\027[1;32m%s\027[0m" x
let start_bright_white = fmt "\027[1;37m"
let start_white = fmt "\027[37m"
let end_color = "\027[0m"
let italic x  = "\027[3m" ^ x ^ "\027[0m"
let underline x  = "\027[4m" ^ x ^ "\027[0m"
let blink x  = "\027[5m" ^ x ^ "\027[0m"


let log x = print ("-- " ^ x)

let debug = false

let trace x =
  if debug then print ((cyan " > ") ^ x)
           else ()

let (!!) _= print "* * *"

let warn x =
  if debug then print ((yellow " ! ") ^ x)
           else ()

let fail = failwith

module Lazy_stream = struct
  type 'a t = Cons of 'a * 'a t Lazy.t | Nil

  let of_stream stream =
    let rec next stream =
      try Cons(Stream.next stream, lazy (next stream))
      with Stream.Failure -> Nil
    in
    next stream

  let of_function f =
    let rec next f =
      match f () with
      | Some x -> Cons(x, lazy (next f))
      | None -> Nil
    in
    next f

  let of_string str = str |> Stream.of_string |> of_stream
  let of_channel ic = ic |> Stream.of_channel |> of_stream
end


let (<~>) x y = match compare x y with
  |  0 -> `EQ
  |  1 -> `GT
  | -1 -> `LT
  |  _ -> raise (Failure "Impossible comparison result.")

module Opt = struct
  exception No_value of string

  let value self ~default =
    match self with
    | Some s -> s
    | None   -> default

  let value_exn ?(msg = "option has no value") self =
    match self with
    | Some self -> self
    | None      -> raise (No_value msg)

  let map self ~f =
    match self with
    | Some self -> Some (f self)
    | None   -> None

  let value_map self ~default ~f =
    match self with
    | Some s -> f s
    | None   -> default

  let return x = Some x

  let bind self ~f =
    match self with
    | Some x -> f x
    | None   -> None

  let is_some = function
    | Some _ -> true
    | None   -> false

  let is_none = function
    | Some _ -> false
    | None   -> true

  let (>>=) self f = bind self ~f

  let (!!) self = value_exn self

  let (||) self default = value self ~default
end

let (!!) = Opt.(!!)
let (||) = Opt.(||)


module Show = struct
  let int = string_of_int
  let float = string_of_float
  let list show_item l = fmt "[%s]" (String.concat ", " (List.map show_item l))
  let assoc k v l = list (fun (x, y) -> fmt "(%s, %s)" (k x) (v y)) l
  let option show_item opt =
    match opt with
    | Some item -> fmt "Some %s" (show_item item)
    | None -> "None"
end


;;

module Dict = struct
  type ('a, 'b) t = ('a * 'b) list
    [@@deriving show]

  let s = show
  let find t key =
    let rec loop = function
      | [] -> None
      | (k, v) :: l -> if k = key then Some v else loop l
    in
    loop t

  let mem t key = find t key <> None

  let remove t key =
    List.filter (fun (key', _) -> not (key = key')) t

  let add t key value =
    (key, value) :: remove t key

  let inverse t = List.map (fun (x, y) -> (y, x)) t

  let map f t = List.map (fun (key, value) -> (key, f value)) t
end
