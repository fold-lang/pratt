
open Pratt_foundation
open Pratt_syntax

(* type ('a, 's) parser = 's -> ('a * 's, string) result *)

module Make(S : sig type t end) = struct
  type state = S.t

  type error =
    | Internal of string

  type 'a t = state -> ('a * state, error) result

  let get =
    fun state -> Ok (state, state)

  let put state =
    fun _ -> Ok ((), state)

  let zero =
    fun s -> Ok ((), s)

  let error msg =
    fun _ -> Error (Internal msg)

  let return x =
    fun s -> Ok (x, s)

  let (>>=) p f =
    fun s ->
      match p s with
      | Ok (x, s') -> (f x) s'
      | Error e    -> Error e

  let (>>) p q = p >>= fun _ -> q
  let (<<) p q = p >>= fun x -> q >>= fun _ -> return x

  let (<|>) p q =
    fun s ->
      match p s with
      | Error m -> q s
      | Ok x    -> Ok x

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
end

