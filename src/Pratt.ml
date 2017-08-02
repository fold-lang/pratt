
open Pure

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
  | Empty

let unexpected ?expected ?actual () =
  Unexpected { expected; actual }

let error_to_string pp_token = function
  | Unexpected { expected = Some t1; actual = Some t2 } ->
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
  | Empty ->
    Fmt.strf "Syntax error: empty parser result"

let pp_error pp_token ppf = function
  | Unexpected { expected; actual } ->
    Fmt.pf ppf "@[<2>Unexpected@ {@ expected =@ @[%a@];@ actual =@ @[%a@] }@]"
      (Fmt.Dump.option pp_token) expected (Fmt.Dump.option pp_token) actual
  | Invalid_infix token ->
    Fmt.pf ppf "@[<2>Invalid_infix@ @[%a@] @]" pp_token token
  | Invalid_prefix token ->
    Fmt.pf ppf "@[<2>Invalid_prefix@ @[%a@] @]" pp_token token
  | Empty -> Fmt.pf ppf "Empty"

type 't state =
  { lexer : 't Iter.t;
    token : 't option }

type ('t, 'a) parser = 't state -> ('a * 't state, 't error) result

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


let zero = fun _input -> Error Empty

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
    get >>= fun state ->
    match Iter.view state.lexer with
    | Some (token, lexer) -> put { lexer; token = Some token }
    | None -> put { state with token = None } in
  p s

let current = fun s ->
  let p = get >>= fun { token } ->
    match token with
    | Some x -> return x
    | None -> error (unexpected ()) in
  p s

let next s =
  let p =
    current >>= fun x ->
    advance >>= fun () -> return x in
  p s

let expect (expected : 't) =
  get >>= fun { token } ->
  match token with
  | Some actual when actual = expected -> return actual
  | Some actual -> error (unexpected ~actual ~expected ())
  | None -> error (unexpected ~expected ())

let consume tok =
  expect tok >>= fun _ -> advance

let exactly x =
  expect x >>= fun x -> advance >>= fun () -> return x

let satisfy test =
  next >>= function
  | actual when test actual -> return actual
  | actual -> error (unexpected ~actual ())

let any s = (satisfy (const true)) s

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


type ('t, 'a) grammar = {
  term : ('t, 'a) term;
  null : ('t, ('t, 'a) null) Hash_map.t;
  left : ('t, ('t, 'a) left) Hash_map.t;
}

and ('t, 'a) term = ('t, 'a) parser
and ('t, 'a) null = (('t, 'a) grammar ->       ('t, 'a) parser)
and ('t, 'a) left = (('t, 'a) grammar -> 'a -> ('t, 'a) parser) * int

type ('t, 'a) rule =
  | Term of      ('t, 'a) term
  | Null of 't * ('t, 'a) null
  | Left of 't * ('t, 'a) left

module Grammar = struct
  type ('t, 'a) t = ('a, 'a) grammar

  let make () =
    { term = (current >>= fun t -> error (Invalid_prefix t));
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
    Hash_map.iter (fun t _ -> Fmt.pr "%a\n" pp_token t) self.left;
end

let rec nud grammar rbp =
  current >>= fun token ->
  let parse =
    match Hash_map.get grammar.null token with
    | Some p -> p
    | None -> (fun g -> grammar.term) in
  parse grammar >>= led grammar rbp

and led grammar rbp x =
  get >>= fun { token = token_opt } ->
  match token_opt with
  | Some token ->
    begin match Hash_map.get grammar.left token with
      | Some (parse, lbp) ->
        if lbp > rbp then
          parse grammar x >>= led grammar rbp
        else
          return x
      | None -> error (Invalid_infix token)
    end
  | None -> return x

let parse rules =
  let grammar = List.fold_left
      Grammar.add (Grammar.make ()) rules in
  nud grammar 0

let run p input =
  let token, lexer =
    match Iter.view input with
    | Some (t, input') -> Some t, input'
    | None -> None, input in
  let state = { lexer; token } in
  match p state with
  | Ok (x, _) -> Ok x
  | Error e -> Error e



  (* let token f = *)
  (*   let parse token grammar = *)
  (*     match f token with *)
  (*     | Some x -> advance >>= fun () -> return x *)
  (*     | None -> error (Invalid_prefix token) in *)
  (*   Atom parse *)

let term parse =
  Term parse

let infix precedence token f =
  let parse grammar x =
    advance >>= fun () ->
      nud grammar precedence >>= fun y ->
        return (f x y) in
  Left (token, (parse, precedence))

let infixr precedence token f =
  let parse grammar x =
    advance >>= fun () ->
      nud grammar (precedence - 1) >>= fun y ->
        return (f x y) in
  Left (token, (parse, precedence))

let prefix token f =
  let parse grammar =
    advance >>= fun () ->
      nud grammar 0 >>= fun x ->
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
      nud grammar 0 >>= fun x ->
        consume token2 >>= fun () ->
          return (f x) in
  Null (token1, parse)

let delimiter token =
  let parse g x = error (Invalid_infix token) in
  Left (token, (parse, 0))

