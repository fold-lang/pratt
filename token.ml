

type k = Symbol | Number | End

type t = { kind : k; text : string }

let to_string t =
  let k = match t.kind with
  | Symbol -> "#"
  | Number -> ""
  | End -> "#end" in
  Printf.sprintf "%s%s" k t.text

let is_end x =
    match x.kind with
    | End -> true
    | _ -> false


let print t = print_endline (to_string t)

let print_list tl =
  print_string "[";
  List.iter (fun t -> print_string @@ " " ^ (to_string t)) tl;
  print_endline " ]";

