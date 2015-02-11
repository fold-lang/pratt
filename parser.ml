
open Foundation
open Lexicon
open Syntax

(* -- Parser Definition -- *)

type 'a parser = state -> ('a * state) result

and state =
  { input   : token_stream;
    grammar : token -> symbol;
    symbol  : symbol }

and symbol =
  { tok : token;
    lbp : int;
    led : (expr -> expr parser) option;
    nud : (expr parser) option }

let run p     = fun s -> p s
let get       = fun s -> Ok (s, s)
let put s     = fun _ -> Ok ((), s)
let zero      = fun s -> Ok ((), s)
let error msg = fun _ -> Error msg


(* -- Monad Operations -- *)

let return x  = fun s -> Ok (x, s)

let (>>=) p f = fun s ->
    match p s with
    | Ok (x, s') -> (f x) s'
    | Error msg  -> Error msg

let (>>) p q = p >>= fun _ -> q
let (<<) p q = p >>= fun x -> q >>= fun _ -> return x


(* -- Base Combinators -- *)

let (<|>) p q = fun s ->
    match p s with
    | Error m -> begin match q s with
        | Error _ -> Error m
        | ok -> ok
    end
    | ok -> ok

let advance = get >>= fun s ->
    let token  = read_token s.input in
    let symbol = s.grammar token in
    put { s with symbol = symbol }

let consume = advance >> get

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
                else error ""


(* -- Singleton Combinators -- *)

let exactly x  = satisfy ((=) x)
let one_of  xs = satisfy (fun x -> List.mem x xs)
let none_of xs = satisfy (fun x -> not (List.mem x xs))
let range s e  = satisfy (fun x -> s <= x && x <= e)


(* -- Tokenizers -- *)

let space     = one_of [' '; '\t'; '\r'; '\n']
let spaces    = skip_many space
let newline   = exactly '\n'
let tab       = exactly '\t'
let upper     = range 'A' 'Z'
let lower     = range 'a' 'z'
let digit     = range '0' '9'
let letter    = lower  <|> upper
let alpha_num = letter <|> digit
let hex_digit = range 'a' 'f' <|> range 'A' 'F'
let oct_digit = range '0' '7'

(* -- Error Handling -- *)

let led_error t =
    error (format "%s: symbol %s takes no arguments."
            (show_location t.location) (show_literal t.value))

let nud_error t =
    error (format "%s: symbol %s requires a left argument."
            (show_location t.location) (show_literal t.value))

