

type k = Symbol | Number | Eof

type t = { kind : k; text : string }

let eof = { kind = Eof ; text = "" }


let to_string t =
  let k = match t.kind with
  | Symbol -> "symbol"
  | Number -> "number"
  | Eof -> "eof" in
  Printf.sprintf "(%s %s)" k t.text
