
open Proto

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
  type t =
    | Symbol of string
    | Identifier of string
    | String of string
    | Int of int
    | Bool of bool
    | Object of (string * t) list
    | Function of string option * string list * t list
    | Ternary of t * t * t
    | Call of t * t list
    | Binary of string * t * t
    | Unary of string * t
    | Return of t
    | Var of string * t

  let rec pp formatter self =
    match self with
    | Symbol x | Identifier x ->
      Fmt.pf formatter "%s" x

    | String x ->
      Fmt.pf formatter "\"%s\"" x

    | Bool x ->
      Fmt.pf formatter "%b" x

    | Int x ->
      Fmt.pf formatter "%d" x

    | Return (Function (fname, args, body)) ->
      Fmt.pf formatter "@[<v4>return function%s(@[<hv>%a@]) {@,%a@]@,}"
        (option ((^) " ") (always "") fname)
        Fmt.(list ~sep:(always ", @,") string) args
        Fmt.(list ~sep:(always "@,") pp) body

    | Return x ->
      Fmt.pf formatter "return %a" pp x

    | Function (name_opt, args, body) ->
      Fmt.pf formatter "@[<v>@[<v4>function%s(@[<hv>%a@]) {@,%a@]@,@]}"
        (option ((^) " ") (always "") name_opt)
        Fmt.(list ~sep:(always ", @,") string) args
        Fmt.(list ~sep:(always "@,") pp) body

    | Object o ->
      let open Fmt in
      let key = prefix (always "\"") string |> suffix (always "\"") in
      pf formatter "@[<4>{%a@]}"
             (list ~sep:(always ",@ ")
                (pair ~sep:(always ":@ ") key pp)) o

    | Call (f, xs) ->
      Fmt.pf formatter "@[<4>%a(@,%a@]@,)" pp f Fmt.(list ~sep:(always ",@ ") pp) xs

    | Ternary (t, a, b) ->
      Fmt.pf formatter "@[<4>%a@ @,?@ %a@, :@ %a@]" pp t pp a pp b

    | Binary (op, a, b) ->
      Fmt.pf formatter "@[<4>%a@, %s@ %a@]" pp a op pp b

    | Unary (op, a) ->
      Fmt.pf formatter "%s%a" op pp a

    | Var (name, Function (fname, args, body)) ->
      Fmt.pf formatter "@[<v4>var %s = function%s(@[<hv>%a@]) {@,%a@]@,}"
        name (option ((^) " ") (always "") fname)
        Fmt.(list ~sep:(always ", @,") string) args
        Fmt.(list ~sep:(always "@,") pp) body

    | Var (name, value) ->
      Fmt.pf formatter "@[<4>var %s =@ %a@]" name pp value
end

let literal g : AST.t P.parser =
  P.current >>= function
  | L.Identifier x -> P.advance >>= fun () -> return (AST.Identifier x)
  | L.String x -> P.advance >>= fun () -> return (AST.String x)
  | L.Int x -> P.advance >>= fun () -> return (AST.Int x)
  | L.Bool x -> P.advance >>= fun () -> return (AST.Bool x)
  | t -> P.error (P.unexpected_token t)

let identifier : string P.parser =
  P.current >>= function
  | L.Identifier x -> P.advance >>= fun () -> return x
  | t -> P.error (P.unexpected_token t)

let string : string P.parser =
  P.current >>= function
  | L.String x -> P.advance >>= fun () -> return x
  | t -> P.error (P.unexpected_token t)

module Parser = struct
  let return' grammar =
    P.consume (L.Keyword "return") >>= fun () ->
    P.parse grammar >>= fun x ->
    return (AST.Return x)

  let var grammar =
    P.consume (L.Keyword "var") >>= fun () ->
    identifier >>= fun name ->
    P.current >>= fun current_token ->
    P.consume (L.Delimeter "=") >>= fun () ->
    P.parse grammar >>= fun value ->
    return (AST.Var (name, value))

  let function' grammar =
    let local = P.Grammar.new_scope grammar in
    let local = P.Grammar.add local (P.null (L.Keyword "return") return') in
    let local = P.Grammar.add local (P.term literal) in
    P.consume (L.Keyword "function") >>= fun () ->
    P.default None (identifier >>= (return << Option.some)) >>= fun name_opt ->
    P.consume (L.Delimeter "(") >>= fun () ->
    P.default [] (identifier |> separated_by (P.consume (L.Delimeter ","))) >>= fun args ->
    P.consume (L.Delimeter ")") >>= fun () ->
    P.consume (L.Delimeter "{") >>= fun () ->
    P.many (P.parse local) >>= fun body ->
    P.consume (L.Delimeter "}") >>= fun () ->
    return (AST.Function (name_opt, args, body))

  let parse = P.parse @@ P.grammar [
    P.term literal;
    (* XXX: WTF? *)
    (* TODO: Add context: "while parsing `var` expected x but got y". *)
    (* P.null (L.Keyword "var") (fun g -> P.consume (L.Keyword "varx") >>= fun () -> return (AST.Int 42)); *)
    P.null (L.Keyword "var") var;
    P.null (L.Keyword "function") function';
    P.delimiter (L.Delimeter "}");
    P.delimiter (L.Delimeter "=");
    P.delimiter (L.Delimeter ":");
    P.delimiter (L.Delimeter ",");
    P.delimiter (L.Delimeter ")");


    P.left 30 (L.Operator "+") (P.binary (fun a b -> (AST.Binary ("+", a, b))));
    P.null (L.Operator "+") (P.unary (fun a -> (AST.Unary ("+", a))));

    P.null (L.Delimeter "{") begin fun grammar ->
      P.consume (L.Delimeter "{") >>= fun () ->
      let item = pair ~sep:(P.consume (L.Delimeter ":")) string (P.parse grammar) in
      P.default [] (item |> separated_by (P.consume (L.Delimeter ","))) >>= fun args ->
      P.consume (L.Delimeter "}") >>= fun () ->
      return (AST.Object args)
    end;

    P.left 80 (L.Delimeter "(") begin fun grammar f ->
      P.consume (L.Delimeter "(") >>= fun () ->
      P.default [] (P.parse grammar |> separated_by (P.consume (L.Delimeter ","))) >>= fun args ->
      P.consume (L.Delimeter ")") >>= fun () ->
      return (AST.Call (f, args))
    end;

    P.left 20 (L.Operator "?") begin fun grammar condition ->
      P.consume (L.Operator "?") >>= fun () ->
      P.parse grammar >>= fun consequence ->
      P.consume (L.Delimeter ":") >>= fun () ->
      P.parse grammar >>= fun alternative ->
      return (AST.Ternary (condition, consequence, alternative))
    end;
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

var sum = function(x, y) { return x + y }

var partialSum = function(x) { return function (y) { return x + y } }

var x = cond(a, f(1, 2, true), g()) ? "yes" : "no"

var point = {
  "x": 42,
  "y": 11
}
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

