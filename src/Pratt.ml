
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
    Fmt.strf "Syntax error: token '%a' cannot be used in prefix postion" pp_token token
  | Invalid_prefix token ->
    Fmt.strf "Syntax error: token '%a' cannot be used in infix position" pp_token token
  | Empty ->
    Fmt.strf "Syntax error: empty parser result"

let pp_error pp_token ppf = function
  | Unexpected { expected; actual } ->
    Fmt.pf ppf "@[<2>Unexpected@ {@ expected =@ @[%a@];@ actual =@ @[%a@] }@]"
      (Fmt.option pp_token) expected (Fmt.option pp_token) actual
  | Invalid_infix token ->
    Fmt.pf ppf "@[<2>Invalid_infix@ @[%a@] @]" pp_token token
  | Invalid_prefix token ->
    Fmt.pf ppf "@[<2>Invalid_infix@ @[%a@] @]" pp_token token
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
  | Error Empty  -> q input
  | Error e -> Error e


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
    | Some (_, lexer) -> put { state with lexer }
    | None -> return () in
  p s

let current = fun s ->
  let p = get >>= fun { token } ->
    match token with
    | Some x -> return x
    | None -> error (unexpected ()) in
  p s

let expect (expected : 't) =
  current >>= function
  | actual when actual = expected -> return actual
  | actual -> error (unexpected ~actual ~expected ())

let consume tok =
  expect tok >>= fun _ -> advance

let exactly x =
  expect x >>= fun x -> advance >>= fun () -> return x

let satisfy test =
  current >>= function
  | actual when test actual -> return actual
  | actual -> error (unexpected ~actual ())

let any s = (satisfy (const true)) s

let from list =
  satisfy (fun x -> List.mem x list)

let none list =
  satisfy (fun x -> not (List.mem x list))


type ('t, 'a) atom = ('t ->       ('t, 'a) parser)
type ('t, 'a) null = ('a ->       ('t, 'a) parser)
type ('t, 'a) left = ('a -> 'a -> ('t, 'a) parser) * int

type ('t, 'a) rule =
  | Atom of      ('t, 'a) atom
  | Null of 't * ('t, 'a) null
  | Left of 't * ('t, 'a) left

type ('t, 'a) grammar = {
  atom : ('t, 'a) atom;
  null : ('t, ('t, 'a) null) Hashtbl.t;
  left : ('t, ('t, 'a) left) Hashtbl.t;
}

let get_null token grammar =
  Hashtbl.find grammar.null token

let get_left token grammar =
  Hashtbl.find grammar.left token

let get_atom token grammar =
  grammar.atom

let null grammar precedence =
  (* current >>= fun token -> *)
  undefined ()

let parse grammar =
  null grammar 0

let run p input =
  let token, lexer =
    match Iter.view input with
    | Some (t, input') -> Some t, input'
    | None -> None, input in
  let state = { lexer; token } in
  match p state with
  | Ok (x, _) -> Ok x
  | Error e -> Error e

module Rule = struct
  type ('t, 'a) t = ('t, 'a) rule

  let token f =
    let parse token =
      match f token with
      | Some x -> return x
      | None -> error (Invalid_prefix token) in
    Atom parse

  let infix precedence token f =
    let parse grammar x =
      advance >>= fun () ->
      null grammar precedence >>= fun y ->
      return (f x y) in
    Left (token, (parse, precedence))

  let infixr precedence token f =
    let parse grammar x =
      advance >>= fun () ->
      null grammar (precedence - 1) >>= fun y ->
      return (f x y) in
    Left (token, (parse, precedence))

  let prefix token f =
    let parse grammar =
      advance >>= fun () ->
      null grammar 0 >>= fun x ->
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
      null grammar 0 >>= fun x ->
      consume token2 >>= fun () ->
      return (f x) in
    Null (token1, parse)

  let delimiter token =
    let parse g x = error (Invalid_infix token) in
    Left (token, (parse, 0))
end

