

type k = Symbol | Number | Eof

type t = { kind : k; text : string }

let eof = { kind = Eof ; text = "" }


let to_string t =
  let k = match t.kind with
  | Symbol -> "#"
  | Number -> ""
  | Eof -> "#eof" in
  Printf.sprintf "%s%s" k t.text

let print t = print_endline (to_string t)

let print_list tl =
  print_string "[";
  List.iter (fun t -> print_string @@ " " ^ (to_string t)) tl;
  print_endline " ]";

