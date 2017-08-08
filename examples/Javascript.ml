
open Pratt




type syntax = [
  | `Identifier of string
  | `String of string
  | `Function of string * string list * syntax list
  | `Return of syntax
  | `Var of string * syntax
]


let list value_printer formatter =
  let rec loop values =
    match values with
    | [] -> ()
    | [x] ->
      value_printer formatter x;
      Fmt.pf formatter "@.";
    | x :: xs ->
      value_printer formatter x;
      Fmt.pf formatter "@.";
      loop xs in
  loop

module Syntax = struct
  type t = syntax

  let rec printer formatter syntax =
    match syntax with
    | `Identifier x -> Fmt.pf formatter "%s" x
    | `String x -> Fmt.pf formatter "\"%s\"" x
    | `Return x -> Fmt.pf formatter "return %a" printer x
    | `Function (name, args, body) ->
      Fmt.(pf formatter "@[<v4>function %s(@[<v>%a@]) {@,%a@]@,}"
             name (list string) args (list printer) body)
    | `Var (name, value) ->
      Fmt.pf formatter "@[<h>var %s =@, %a@]" name printer value
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

let literal : (token, syntax) parser =
  next >>= function
  | `Identifier x -> return (`Identifier x)
  | `String x -> return (`String x)
  | t -> error (unexpected_token t)

let identifier : (token, string) parser =
  next >>= function
  | `Identifier x -> return x
  | t -> error (unexpected_token t)

let symbol x  = consume (`Symbol x)
let keyword x = consume (`Keyword x)

let parse_return grammar =
  keyword "return" >>= fun () ->
  parse grammar >>= fun x ->
  return (`Return x)

let parse_var grammar =
  keyword "var" >>= fun () ->
  identifier >>= fun name ->
  symbol "=" >>= fun () ->
  parse grammar >>= fun value ->
  let res = `Var (name, value) in
  return res

let parse_function grammar =
  keyword "function" >>= fun () ->
  identifier >>= fun name ->
  symbol "(" >>= fun () ->
  identifier >>= fun arg ->
  symbol ")" >>= fun () ->
  symbol "{" >>= fun () ->
  many (parse grammar) >>= fun body ->
  symbol "}" >>= fun () ->
  return (`Function (name, [arg], body))

let js = grammar [
  term literal;
  delimiter (`Symbol "}");
  delimiter (`Keyword "return");
  delimiter (`Keyword "function");
  delimiter (`Keyword "var");
  delimiter (`Symbol "=");
  rule (`Keyword "var")      parse_var;
  rule (`Keyword "function") parse_function;
  rule (`Keyword "return")   parse_return;
]

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
    let parser = parse js in
    match run parser input with
    | Ok (result, input') ->
      Fmt.pr "%a\n" Syntax.printer result;
      loop input'
    | Error Empty -> ()
    | Error e -> Fmt.pr "main: %s\n\n" (error_to_string pp_token e) in
  loop (Iter.list program)


