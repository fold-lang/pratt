
open Pure

module P = Pratt
module L = Lexer

let (>>=) = P.(>>=)
let return = P.return


let separated_by sep p =
  p >>= fun x ->
  P.many (sep >>= fun () -> p) >>= fun xs ->
  return (x :: xs)


module AST = struct
  type t =
    | Symbol of string
    | Identifier of string
    | String of string
    | Int of int
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

    | Int x ->
      Fmt.pf formatter "%d" x

    | Return x ->
      Fmt.pf formatter "return %a" pp x

    | Function (name_opt, args, body) ->
      Fmt.pf formatter "@[<v>@[<v4>function%s(@[<hv>%a@]) {@,%a@]@,@]}"
        (option "" ((^) " ") name_opt)
        Fmt.(list ~sep:(always ", @,") string) args
        Fmt.(list ~sep:(always "@,") pp) body

    | Call (f, xs) ->
      Fmt.pf formatter "@[<4>%a(@,%a@]@,)" pp f Fmt.(list ~sep:(always ",@ ") pp) xs

    | Ternary (t, a, b) ->
      Fmt.pf formatter "@[<4>%a@ @,?@ %a@, :@ %a@]" pp t pp a pp b

    | Binary (op, a, b) ->
      Fmt.pf formatter "@[<4>%a@, %s@ %a@]" pp a op pp b

    | Unary (op, a) ->
      Fmt.pf formatter "%s%a" op pp a

    | Var (name, value) ->
      Fmt.pf formatter "@[<4>var %s =@ %a@]" name pp value
end

let literal g : (Lexer.token, AST.t) P.parser =
  P.next >>= function
  | L.Identifier x -> return (AST.Identifier x)
  | L.String x -> return (AST.String x)
  | L.Int x -> return (AST.Int x)
  | t -> P.error (P.unexpected_token t)

let identifier : (Lexer.token, string) P.parser =
  P.current >>= function
  | L.Identifier x -> P.advance >>= fun () -> return x
  | t -> P.error (P.unexpected_token t)

module Parser = struct
  let keyword x  = P.consume (L.Keyword x)
  let delimiter x  = P.consume (L.Delimeter x)
  let operator x  = P.consume (L.Operator x)

  let return' grammar =
    keyword "return" >>= fun () ->
    P.parse grammar >>= fun x ->
    return (AST.Return x)

  let var grammar =
    keyword "var" >>= fun () ->
    identifier >>= fun name ->
    operator "=" >>= fun () ->
    P.parse grammar >>= fun value ->
    let res = AST.Var (name, value) in
    return res

  let function' grammar =
    keyword "function" >>= fun () ->
    P.default None (identifier >>= (return << Option.some)) >>= fun name_opt ->
    delimiter "(" >>= fun () ->
    P.default [] (identifier |> separated_by (delimiter ",")) >>= fun args ->
    delimiter ")" >>= fun () ->
    delimiter "{" >>= fun () ->
    P.many (P.parse grammar) >>= fun body ->
    delimiter "}" >>= fun () ->
    return (AST.Function (name_opt, args, body))

  let parse = P.parse <| P.grammar [
    P.term literal;
    P.null (L.Keyword "var") var;
    P.null (L.Keyword "function") function';
    P.null (L.Keyword "return") return';
    P.delimiter (L.Delimeter "}");
    P.delimiter (L.Delimeter "=");
    P.delimiter (L.Delimeter ",");
    P.delimiter (L.Delimeter "(");
    P.delimiter (L.Delimeter ")");


    P.left 30 (L.Operator "+") (P.binary (fun a b -> (AST.Binary ("+", a, b))));
    P.null (L.Operator "+") (P.unary (fun a -> (AST.Unary ("+", a))));

    P.left 20 (L.Operator "?") begin fun g condition ->
      P.consume (L.Operator "?") >>= fun () ->
      P.parse g >>= fun consequence ->
      P.consume (L.Operator ":") >>= fun () ->
      P.parse g >>= fun alternative ->
      return (AST.Ternary (condition, consequence, alternative))
    end;
  ]
end


let input = {|
function hello(name, a) {
    var x = name ? name : "Hello, world!"
    return x
}
var y = 42 + 0
var z = +4

var sum = function(x, y) { return x + y }

var partial_sum = function(x) { return function (y) { return x + y } }

|}

let main =
  let rec loop input =
    if not (P.Stream.is_empty input) then
    match P.run Parser.parse input with
      | Ok (result, input') ->
        Fmt.pr "%a@." AST.pp result;
        loop input'
      | Error Zero -> ()
      | Error e -> Fmt.pr "main: %s@." (P.error_to_string Lexer.pp_token e) in
  loop (Lexer.(to_stream (of_string input)))

