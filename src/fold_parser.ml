
open Foundation
open Fold_syntax

type ('a, 's) parser = 's -> ('a * 's) result

let run p     = fun s -> p s
let get       = fun s -> Ok (s, s)
let put s     = fun _ -> Ok ((), s)
let zero      = fun s -> Ok ((), s)

let error msg = fun _ -> Error msg

let return x  = fun s -> Ok (x, s)

let (>>=) p f = fun s ->
    match p s with
    | Ok (x, s') -> (f x) s'
    | Error msg  -> Error msg

let (>>|) p f = fun s ->
    match p s with
    | Ok (x, s') -> Ok ((f x), s')
    | Error msg  -> Error msg

let (>>) p q = p >>= fun _ -> q
let (<<) p q = p >>= fun x -> q >>= fun _ -> return x

let (<|>) p q = fun s ->
  match p s with
  | Error m -> q s
  | ok -> ok

let inspect f = get >>= fun s ->
    f s; put s

let between op ed x = op >> x << ed

let option x p = p <|> return x
let optional p = option () (p >> return ())

let rec skip_many x = optional (x >>= fun _ -> skip_many x)

let rec many p =
    option [] (p >>= fun x  -> many p
                 >>= fun xs -> return (x :: xs))

let satisfy test =
    get >>= fun x ->
    if (test x) then return x
                else error "could not satisfy test"

let exactly x  = satisfy ((=) x)
let one_of  xs = satisfy (fun x -> List.mem x xs)
let none_of xs = satisfy (fun x -> not (List.mem x xs))
let range s e  = satisfy (fun x -> s <= x && x <= e)

