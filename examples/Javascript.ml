
open Proto
open Astring

module Stream = Pratt.Stream
module P = Pratt.Make(Lexer.Token)
module L = Lexer

let (>>=) = P.(>>=)
let return = P.return

let separated_by sep p =
  p >>= fun x ->
  P.many (sep >>= fun () -> p) >>= fun xs ->
  return (x :: xs)

let pair ~sep p1 p2 =
  p1 >>= fun x ->
  sep >>= fun () ->
  p2 >>= fun y ->
  return (x, y)


module AST = struct
  type t = [
    | `Binary of string * t * t
    | `Bool of bool
    | `Call of t * t list
    | `Dot of t * t
    | `Function of string * string list * t list
    | `Identifier of string
    | `Int of int
    | `Lambda of string list * t list
    | `Object of (string * t) list
    | `Return of t
    | `String of string
    | `Symbol of string
    | `Ternary of t * t * t
    | `Unary of string * t
    | `Var of string * t
  ]

  let rec pp formatter self =
    let open Fmt in
    let pp_args = vbox (list ~sep:(always ", ") string) in
    let pp_body = vbox (list pp) in
    let pp_item = pair ~sep:(always ":@ ") String.dump pp in
    match self with
    | `Symbol x | `Identifier x -> String.pp formatter x
    | `String x -> String.dump formatter x
    | `Bool x -> Bool.pp formatter x
    | `Int x -> Int.pp formatter x

    | `Return (`Lambda (args, body)) ->
      pf formatter "@[<v4>return function(@[%a@]) {@,%a@]@,}"
        pp_args args pp_body body

    | `Return x ->
      Fmt.pf formatter "return @[%a@]" pp x

    | `Lambda (args, body) ->
      pf formatter "@[<v4>function(@[%a@]) {@,%a@]@,}"
        pp_args args pp_body body

    | `Function (name, args, body) ->
      pf formatter "@[<v4>function %s(@[%a@]) {@,%a@]@,}"
        name pp_args args pp_body body

    | `Object o ->
      let open Fmt in
      pf formatter "@[<4>{%a@]}" (list ~sep:(always ",@ ") pp_item) o

    | `Call (f, xs) ->
      Fmt.pf formatter "%a(@[%a@])" pp f Fmt.(list ~sep:(always ",@ ") pp) xs

    | `Ternary (t, a, b) ->
      Fmt.pf formatter "@[<4>%a@ @,?@ %a@, :@ %a@]" pp t pp a pp b

    | `Binary (op, a, b) ->
      Fmt.pf formatter "@[<4>%a@,@ %s@ %a@]" pp a op pp b

    | `Unary (op, a) ->
      Fmt.pf formatter "%s%a" op pp a

    | `Var (name, `Lambda (args, body)) ->
      pf formatter "@[<v4>var %s = function(@[%a@]) {@,%a@]@,}"
        name pp_args args pp_body body

    | `Var (name, value) ->
      Fmt.pf formatter "@[<4>var %s =@ %a@]" name pp value

    | `Dot (x, y) ->
      Fmt.pf formatter "%a.%a" pp x pp y
end

let literal g : AST.t P.parser =
  P.current >>= function
  | `Identifier x -> P.advance >>= fun () -> return (`Identifier x)
  | `String     x -> P.advance >>= fun () -> return (`String x)
  | `Int        x -> P.advance >>= fun () -> return (`Int x)
  | `Bool       x -> P.advance >>= fun () -> return (`Bool x)
  | t -> P.error (P.unexpected_token t)


module Parser = struct
  let delimiter str = P.consume (`Delimiter str)
  let keyword   str = P.consume (`Keyword   str)

  let filter_map f =
    P.current >>= fun token ->
    match f token with
    | Some x -> P.advance >>= fun () -> return x
    | None -> P.error (P.unexpected_token token)

  let identifier =
    filter_map (function `Identifier x -> Some x | _ -> None)

  let string =
    filter_map (function `String x -> Some x | _ -> None)

  let return' grammar =
    keyword "return" >>= fun () ->
    P.parse grammar >>= fun x ->
    return (`Return x)

  let var grammar =
    keyword "var" >>= fun () ->
    identifier >>= fun name ->
    delimiter "=" >>= fun () ->
    P.parse grammar >>= fun value ->
    return (`Var (name, value))

  let function' grammar =
    let local = P.Grammar.add (P.null (`Keyword "return") return') grammar in
    keyword "function" >>= fun () ->
    P.default "" identifier >>= fun name ->
    delimiter "(" >>= fun () ->
    P.default [] (identifier |> separated_by (delimiter ",")) >>= fun args ->
    delimiter ")" >>= fun () ->
    delimiter "{" >>= fun () ->
    P.many (P.parse local) >>= fun body ->
    delimiter "}" >>= fun () ->
    return (if name = ""
            then `Lambda (args, body)
            else `Function (name, args, body))

  let object' grammar =
    delimiter "{" >>= fun () ->
    let item = pair ~sep:(delimiter ":") string (P.parse grammar) in
    P.default [] (item |> separated_by (delimiter ",")) >>= fun args ->
    delimiter "}" >>= fun () ->
    return (`Object args)

  let call grammar f =
    delimiter "(" >>= fun () ->
    P.default [] (P.parse grammar |> separated_by (delimiter ",")) >>= fun args ->
    delimiter ")" >>= fun () ->
    return (`Call (f, args))

  let ternary grammar condition =
    delimiter "?" >>= fun () ->
    P.parse grammar >>= fun consequence ->
    delimiter ":" >>= fun () ->
    P.parse grammar >>= fun alternative ->
    return (`Ternary (condition, consequence, alternative))

  let parse = P.parse @@ P.grammar [
    P.term literal;
    (* XXX: WTF? *)
    (* TODO: Add context: "while parsing `var` expected x but got y". *)
    (* P.null (`Keyword "var") (fun g -> P.consume (`Keyword "varx") >>= fun () -> return (`Int 42)); *)
    P.null (`Keyword "var") var;
    P.null (`Keyword "function") function';
    P.null (`Delimiter "{") object';
    P.null (`Operator "+") (P.unary (fun a -> (`Unary ("+", a))));
    P.left 30 (`Operator "+") (P.binary (fun a b -> (`Binary ("+", a, b))));
    P.left 85 (`Delimiter ".") (P.binary (fun a b -> (`Dot (a, b))));
    P.left 30 (`Operator "===") (P.binary (fun a b -> (`Binary ("===", a, b))));
    P.left 80 (`Delimiter "(") call;
    P.left 20 (`Delimiter "?") ternary;
  ]
end

(* It is more important to understand what is *NOT* valid than what is valid. *)
let input = {|

var x = name ? name : "Hello, world!"

function hello(name, a) {
    var x = name ? name : "Hello, world!"
    return x
}
var y = 42 + 0
var z = +4

function f(x) {
    console.log("hello")
    console.log(" ")
    console.log("wolrd")
    var y = x + 1
    function sum(a, b) { return a + b }
    return sum(x, y)
}

var sum = function(x, y) { return x + y }

var partialSum = function(x) { return function (y) { return x + y } }

var x = cond(a, f(1, 2, true), g()) ? "yes" : "no"

var point = {
  "x": 42,
  "y": 11
}

assert(point.x === 42)
|}

let main =
  let rec loop input =
    if not (Stream.is_empty input) then
    match P.run Parser.parse input with
      | Ok (result, input') ->
        Fmt.pr "%a@.@." AST.pp result;
        loop input'
      | Error Zero -> ()
      | Error e -> Fmt.pr "main: %s@." (P.error_to_string e) in
  loop (Lexer.(to_stream (of_string input)))

