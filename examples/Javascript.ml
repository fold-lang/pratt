
open Pratt


module Syntax = struct
  type t =
    [ `Identifier of string
    | `String of string
    | `Int of int
    | `Function of string * string list * t list
    | `Ternary of t * t * t
    | `Call of t * t list
    | `Binary of string * t * t
    | `Unary of string * t
    | `Return of t
    | `Var of string * t ]

  let rec pp formatter self =
    match self with
    | `Identifier x ->
      Fmt.pf formatter "%s" x

    | `String x ->
      Fmt.pf formatter "\"%s\"" x

    | `Int x ->
      Fmt.pf formatter "%d" x

    | `Return x ->
      Fmt.pf formatter "return %a" pp x

    | `Function (name, args, body) ->
      Fmt.pf formatter "@[<v4>function %s(@[<v>%a@]) {@,%a@]@,}" name
        Fmt.(list ~sep:(always "@,") string) args
        Fmt.(list ~sep:(always "@,") pp) body

    | `Call (f, xs) ->
      Fmt.pf formatter "@[<4>%a(@,%a@]@,)" pp f Fmt.(list ~sep:(always ",@ ") pp) xs

    | `Ternary (t, a, b) ->
      Fmt.pf formatter "@[<4>%a@ @,?@ %a@, :@ %a@]" pp t pp a pp b

    | `Binary (op, a, b) ->
      Fmt.pf formatter "@[<4>%a@, %s@ %a@]" pp a op pp b

    | `Unary (op, a) ->
      Fmt.pf formatter "%s%a" op pp a

    | `Var (name, value) ->
      Fmt.pf formatter "@[<4>var %s =@ %a@]" name pp value
end

type token = [
  | `Keyword of string
  | `Symbol of string
  | `String of string
  | `Int of int
  | `Identifier of string
]

let pp_token ppf = function
  | `Keyword x -> Fmt.pf ppf "%s" x
  | `Symbol x -> Fmt.pf ppf "%s" x
  | `String x -> Fmt.pf ppf "\"%s\"" x
  | `Int x -> Fmt.pf ppf "%d" x
  | `Identifier x -> Fmt.pf ppf "%s" x

let literal g : (token, Syntax.t) parser =
  next >>= function
  | `Identifier x -> return (`Identifier x)
  | `String x -> return (`String x)
  | `Int x -> return (`Int x)
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
    delimiter (`Symbol ":");
    null (`Keyword "var")    var;
    null (`Keyword "function") function';
    null (`Keyword "return")   return';

    left 30 (`Symbol "+") (binary (fun a b -> (`Binary ("+", a, b))));
    null (`Symbol "+") (unary (fun a -> (`Unary ("+", a))));

    left 20 (`Symbol "?") begin fun g condition ->
      consume (`Symbol "?") >>= fun () ->
      parse g >>= fun consequence ->
      consume (`Symbol ":") >>= fun () ->
      parse g >>= fun alternative ->
      return (`Ternary (condition, consequence, alternative))
    end;
  ]
end

let program = [
  `Keyword "function";
  `Identifier "hello";
  `Symbol "(";
  `Identifier "name";
  `Symbol ")";
  `Symbol "{";
  `Keyword "var"; `Identifier "x"; `Symbol "=";
  `Identifier "name"; `Symbol "?"; `Identifier "name"; `Symbol ":"; `String "Hello, world!";
  `Keyword "return"; `Identifier "x";
  `Symbol "}";

  `Keyword "var"; `Identifier "y"; `Symbol "=";
  `Int 42; `Symbol "+"; `Int 0;

  `Keyword "var"; `Identifier "z"; `Symbol "=";
  `Symbol "+"; `Int 4;
]

let main =
  let rec loop input =
    if Stream.is_empty input then
      Fmt.pr "main: done@."
    else match run Parser.parse input with
      | Ok (result, input') ->
        Fmt.pr "%a@." Syntax.pp result;
        loop input'
      | Error Zero -> ()
      | Error e -> Fmt.pr "main: %s@." (error_to_string pp_token e) in
  loop (Stream.of_list program)

