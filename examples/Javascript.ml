
open Pure
open Pratt


module Syntax = struct
  type t =
    [ `Symbol of string
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
    | `Symbol x ->
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

let literal g : (Lexer.token, Syntax.t) parser =
  next >>= function
  | `Symbol x -> return (`Symbol x)
  | `String x -> return (`String x)
  | `Int x -> return (`Int x)
  | t -> error (unexpected_token t)

let identifier : (Lexer.token, string) parser =
  next >>= function
  | `Symbol x -> return x
  | t -> error (unexpected_token t)

module Parser = struct
  let symbol x  = consume (`Symbol x)

  let return' grammar =
    symbol "return" >>= fun () ->
    parse grammar >>= fun x ->
    return (`Return x)

  let var grammar =
    symbol "var" >>= fun () ->
    identifier >>= fun name ->
    symbol "=" >>= fun () ->
    parse grammar >>= fun value ->
    let res = `Var (name, value) in
    return res

  let function' grammar =
    symbol "function" >>= fun () ->
    identifier >>= fun name ->
    symbol "(" >>= fun () ->
    identifier >>= fun arg ->
    symbol ")" >>= fun () ->
    symbol "{" >>= fun () ->
    many_while (fun t -> not (Grammar.has_left t grammar)) (parse grammar) >>= fun body ->
    symbol "}" >>= fun () ->
    return (`Function (name, [arg], body))


  let parse = Pratt.parse <| grammar [
    term literal;
    null (`Symbol "var")    var;
    null (`Symbol "function") function';
    null (`Symbol "return")   return';
    delimiter (`Symbol "}");

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

let input = {|
function hello(name) {
    var x = name ? name : "Hello, world!"
    return x
}
var y = 42 + 0
var z = +4
|}

let main =
  let rec loop input =
    if Stream.is_empty input then
      Fmt.pr "main: done@."
    else match run Parser.parse input with
      | Ok (result, input') ->
        Fmt.pr "%a@." Syntax.pp result;
        loop input'
      | Error Zero -> ()
      | Error e -> Fmt.pr "main: %s@." (error_to_string Lexer.pp_token e) in
  loop (Lexer.(to_stream (of_string input)))

