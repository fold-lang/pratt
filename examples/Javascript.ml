
open Pratt


module Syntax = struct
  type t =
    [ `Identifier of string
    | `String of string
    | `Function of string * string list * t list
    | `Return of t
    | `Var of string * t ]

  let rec printer formatter self =
    match self with
    | `Identifier x ->
      Fmt.pf formatter "%s" x

    | `String x ->
      Fmt.pf formatter "\"%s\"" x

    | `Return x ->
      Fmt.pf formatter "return %a" printer x

    | `Function (name, args, body) ->
      Fmt.pf formatter "@[<v4>function %s(@[<v>%a@]) {@,%a@]@,}" name
        Fmt.(list ~sep:(always "@,") string) args
        Fmt.(list ~sep:(always "@,") printer) body

    | `Var (name, value) ->
      Fmt.pf formatter "@[<4>var %s =@ %a@]" name printer value
end

type token = [
  | `Keyword of string
  | `Symbol of string
  | `String of string
  | `Identifier of string
]

let pp_token ppf = function
  | `Keyword x -> Fmt.pf ppf "%s" x
  | `Symbol x -> Fmt.pf ppf "%s" x
  | `String x -> Fmt.pf ppf "\"%s\"" x
  | `Identifier x -> Fmt.pf ppf "%s" x

let literal : (token, Syntax.t) parser =
  next >>= function
  | `Identifier x -> return (`Identifier x)
  | `String x -> return (`String x)
  | t -> error (unexpected_token t)

let identifier : (token, string) parser =
  next >>= function
  | `Identifier x -> return x
  | t -> error (unexpected_token t)

module Parser = struct
  let symbol x  = consume (`Symbol x)
  let keyword x = consume (`Keyword x)

  let return' grammar =
    keyword "return" >>= fun () ->
    parse grammar >>= fun x ->
    return (`Return x)

  let var grammar =
    keyword "var" >>= fun () ->
    identifier >>= fun name ->
    symbol "=" >>= fun () ->
    parse grammar >>= fun value ->
    let res = `Var (name, value) in
    return res

  let function' grammar =
    keyword "function" >>= fun () ->
    identifier >>= fun name ->
    symbol "(" >>= fun () ->
    identifier >>= fun arg ->
    symbol ")" >>= fun () ->
    symbol "{" >>= fun () ->
    many (parse grammar) >>= fun body ->
    symbol "}" >>= fun () ->
    return (`Function (name, [arg], body))

  let parse = Pratt.parse <| grammar [
    term literal;
    delimiter (`Symbol "}");
    delimiter (`Keyword "return");
    delimiter (`Keyword "function");
    delimiter (`Keyword "var");
    delimiter (`Symbol "=");
    rule (`Keyword "var")      var;
    rule (`Keyword "function") function';
    rule (`Keyword "return")   return';
  ]
end

(*

  function hello(name) {
      var x = "Hello, world!"
      return x
  }

*)
let program = [
  `Keyword "function";
  `Identifier "hello";
  `Symbol "(";
  `Identifier "name";
  `Symbol ")";
  `Symbol "{";
  `Keyword "var"; `Identifier "x"; `Symbol "="; `String "Hello, world!";
  `Keyword "return"; `Identifier "x";
  `Symbol "}";
]

let main =
  let rec loop input =
    match run Parser.parse input with
    | Ok (result, input') ->
      Fmt.pr "%a\n" Syntax.printer result;
      loop input'
    | Error Empty -> ()
    | Error e -> Fmt.pr "main: %s\n\n" (error_to_string pp_token e) in
  loop (Iter.list program)

