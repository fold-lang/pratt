
open Pure

module Stream = Stream

module Hash_map = struct
  include Hashtbl

  let get tbl x =
    try Some (Hashtbl.find tbl x)
    with Not_found -> None
end


type 't error =
  | Unexpected     of { expected : 't option; actual : 't option }
  | Invalid_infix  of 't
  | Invalid_prefix of 't
  | Zero

let unexpected_token ?expected actual =
  Unexpected {expected; actual = Some actual}

let unexpected_end ?expected () =
  Unexpected {expected; actual = None}

let invalid_prefix t =
  Invalid_prefix t

let invalid_infix t =
  Invalid_infix t

let error_to_string pp_token = function
  | Unexpected {expected = Some t1; actual = Some t2} ->
    Fmt.strf "Syntax error: expected '%a' but got '%a'" pp_token t1 pp_token t2
  | Unexpected  { expected = Some t; actual = None } ->
    Fmt.strf "Syntax error: unexpected end of file while parsing '%a'" pp_token t
  | Unexpected { expected = None; actual = None } ->
    Fmt.strf "Syntax error: unexpected end of file"
  | Unexpected { expected = None; actual = Some t } ->
    Fmt.strf "Syntax error: unexpected token '%a'" pp_token t
  | Invalid_infix token ->
    Fmt.strf "Syntax error: '%a' cannot be used in infix postion" pp_token token
  | Invalid_prefix token ->
    Fmt.strf "Syntax error: '%a' cannot be used in prefix position" pp_token token
  | Zero ->
    Fmt.strf "Syntax error: empty parser result"

let pp_error pp_token ppf = function
  | Unexpected { expected; actual } ->
    Fmt.pf ppf "@[<2>Unexpected@ {@ expected =@ @[%a@];@ actual =@ @[%a@] }@]"
      (Fmt.Dump.option pp_token) expected (Fmt.Dump.option pp_token) actual
  | Invalid_infix token ->
    Fmt.pf ppf "@[<2>Invalid_infix@ @[%a@] @]" pp_token token
  | Invalid_prefix token ->
    Fmt.pf ppf "@[<2>Invalid_prefix@ @[%a@] @]" pp_token token
  | Zero -> Fmt.pf ppf "Empty"

type ('t, 'a) parser = 't Stream.t -> ('a * 't Stream.t, 't error) result

let return x =
  fun input -> Ok (x, input)

let (>>=) p f =
  fun input ->
    match p input with
    | Ok (x, input') ->
      let p' = f x in p' input'
    | Error e -> Error e


let put s = fun _ -> Ok ((), s)
let get   = fun s -> Ok (s, s)


let zero = fun _input -> Error Zero

let (<|>) p q = fun input ->
  match p input with
  | Ok value -> Ok value
  | Error _  -> q input
  (* XXX: What if p consumes input? *)
  (* | Error Empty  -> q input *)
  (* | Error e -> Error e *)


let default x p =
  p <|> return x

let rec many p =
  (p >>= fun x -> many p >>= fun xs -> return (x :: xs))
  |> default []

let combine p1 p2 =
  p1 >>= fun x ->
  p2 >>= fun y -> return (x, y)

let rec some p =
  combine p (many p)

let optional p =
  default () (p >>= fun _ -> return ())

let error e =
  fun _state -> Error e

let advance s =
  let p =
    get >>= fun stream ->
    match Stream.next stream with
    | Some (token, stream') -> put stream'
    | None -> return () in
  p s

let current = fun s ->
  let p = get >>= fun state ->
    match Stream.head state with
    | Some token -> return token
    | None -> error (unexpected_end ()) in
  p s

let next s =
  let p =
    current >>= fun x ->
    advance >>= fun () -> return x in
  p s

let expect expected =
  get >>= fun stream ->
  match Stream.head stream with
  | Some actual when actual = expected -> return actual
  | Some actual -> error (unexpected_token ~expected actual)
  | None -> error (unexpected_end ~expected ())

let consume tok =
  expect tok >>= fun _ -> advance

let exactly x =
  expect x >>= fun x -> advance >>= fun () -> return x

let satisfy test =
  next >>= function
  | actual when test actual -> return actual
  | actual -> error (unexpected_token actual)

let any s = (satisfy (always true)) s

let from list =
  satisfy (fun x -> List.mem x list)

let none list =
  satisfy (fun x -> not (List.mem x list))

let range ?(compare = Pure.compare) s e =
  let (<=) a b = not (compare a b = Comparable.greater) in
  satisfy (fun x -> s <= x && x <= e)

let rec choice ps =
  match ps with
  | [] -> zero
  | p :: ps' -> p <|> choice ps'

let guard = function
  | true  -> return ()
  | false -> zero

let when' test m =
   if test then m
   else return ()

let unless test m =
   if test then return ()
   else m

let many_while test p =
  many (current >>= (guard << test) >>= fun () -> p)


type ('t, 'a) grammar = {
  term : ('t, 'a) null;
  null : ('t, ('t, 'a) null) Hash_map.t;
  left : ('t, ('t, 'a) left) Hash_map.t;
}

and ('t, 'a) null = (('t, 'a) grammar ->       ('t, 'a) parser)
and ('t, 'a) left = (('t, 'a) grammar -> 'a -> ('t, 'a) parser) * int

type ('t, 'a) rule =
  | Term of      ('t, 'a) null
  | Null of 't * ('t, 'a) null
  | Left of 't * ('t, 'a) left

module Grammar = struct
  type ('t, 'a) t = ('t, 'a) grammar

  let make () =
    { term = (fun g -> current >>= fun t -> error (Invalid_prefix t));
      null = Hash_map.create 64;
      left = Hash_map.create 64 }

  let add self rule =
    match rule with
    | Term term -> { self with term }
    | Null (t, rule) ->
      Hash_map.add self.null t rule;
      self
    | Left (t, rule) ->
      Hash_map.add self.left t rule;
      self

  let dump pp_token self =
    Fmt.pr "grammar.null:\n";
    Hash_map.iter (fun t _ -> Fmt.pr "%a\n" pp_token t) self.null;
    Fmt.pr "grammar.left:\n";
    Hash_map.iter (fun t _ -> Fmt.pr "%a\n" pp_token t) self.left

  let has_null token grammar =
    is_some <| Hash_map.get grammar.null token

  let has_left token grammar =
    is_some <| Hash_map.get grammar.left token
end

let nud rbp grammar =
  current >>= fun token ->
  let parse =
    match Hash_map.get grammar.null token with
    | Some p -> p
    | None -> grammar.term in
  parse grammar

let rec led rbp grammar x =
  get >>= fun stream ->
  match Stream.head stream with
  | Some token ->
    begin match Hash_map.get grammar.left token with
      | Some (parse, lbp) ->
        if lbp > rbp then
          parse grammar x >>= led rbp grammar
        else
          return x
      | None ->
        (* Treat as delimiter, _i.e._, break. This is useful for multiple
           top-level statements. In the future allow a custom handler. *)
        (* Previous: error (Invalid_infix token) *)
        return x
    end
  | None ->
    return x

let parse ?precedence:(rbp = 0) grammar =
  nud rbp grammar >>= led rbp grammar

let grammar rules =
  List.fold_left Grammar.add (Grammar.make ()) rules

let run p stream =
  match p stream with
  | Ok (x, stream') -> Ok (x, stream')
  | Error e -> Error e

let rule token parse =
  Null (token, parse)

let term parse =
  Term parse

let infix precedence token f =
  let parse grammar x =
    advance >>= fun () ->
    parse ~precedence grammar >>= fun y ->
    return (f x y) in
  Left (token, (parse, precedence))

let infixr precedence token f =
  let parse grammar x =
    advance >>= fun () ->
    parse ~precedence:(precedence - 1) grammar >>= fun y ->
    return (f x y) in
  Left (token, (parse, precedence))

let prefix token f =
  let parse grammar =
    advance >>= fun () ->
    parse grammar >>= fun x ->
    return (f x) in
  Null (token, parse)

let postfix precedence token f =
  let parse grammar x =
    advance >>= fun () ->
    return (f x) in
  Left (token, (parse, precedence))

let between token1 token2 f =
  let parse grammar =
    advance >>= fun () ->
    parse grammar >>= fun x ->
    consume token2 >>= fun () ->
    return (f x) in
  Null (token1, parse)

let delimiter token =
  let parse g x = error (Invalid_infix token) in
  Left (token, (parse, 0))

let null token parse =
  Null (token, parse)

let left precedence token parse =
  Left (token, (parse, precedence))

let binary f = fun g a ->
  advance >>= fun () ->
  parse g >>= fun b ->
  return (f a b)

let unary f = fun g ->
  advance >>= fun () ->
  parse g >>= fun a ->
  return (f a)
