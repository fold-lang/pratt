
(* Applies `f` to the arguments `x` and `y` in reveresed orded. *)
let flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c =
  fun f x y -> f y x

(* Mathematical composition of functions: `(f ∘ g)(x) = f(g(x))`. *)
let compose f g = fun x -> f (g x)

let invcompose g f = fun x -> f (g x)

(* Infix function composition operator. *)
let ( << ) = compose

(* Infix inverse function composition operator. *)
let ( >> ) = invcompose

type void = Void

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


let rec repeat_until fn limit =
	let x = fn () in
	if (x = limit)
		then []
		else [x] @ repeat_until fn limit


let rec repeat_fn_to fn limit =
	let x = fn () in
	if (x = limit)
		then []
		else [x] @ repeat_fn_to fn limit

let (!!) s =
    print_endline ("= " ^ s); s


let print = print_endline

let printf = Printf.printf

let format = Printf.sprintf

let log x = print ("-- " ^ x)

let error = failwith

let first (x, y) = x
let second (x, y) = y

module type Type = sig
  type t
end

