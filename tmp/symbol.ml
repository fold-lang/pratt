
(*
 * AST Symbol
 * Symbol defines a syntax node and the rules of how it creates the AST tree.
 *)

open Foundation


type 'a parselet =
    [ `Prefix of ('a -> 'a Parser.t)
    | `Infix of (int * ('a Parser.t -> 'a -> 'a Parser.t))
    | `Postfix of ('a Parser.t -> 'a -> 'a Parser.t)
    | `Mixfix of ('a Parser.t -> 'a -> 'a Parser.t)]

type 'a t = {
    name : string;
    handler : 'a handler;
}

let create name handler =
    { name; handler }


let to_string x =
    let handler = match x.handler with
    | `Prefix _ -> "prefix"
    | `Infix (p, _) -> "infix " ^ (string_of_int p) in
    Printf.sprintf "(symbol (%s) %s %s)" x.name handler
