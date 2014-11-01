

type t =
    | Symbol of string
    | Number of string
    | End


let to_string =
  function
  | Symbol x -> "#" ^ x
  | Number x -> x
  | End -> "__end__"


let is_end =
  function End -> true | _ -> false


let print t = print_endline (to_string t)

let print_list tl =
  print_string "[";
  List.iter (fun t -> print_string @@ " " ^ (to_string t)) tl;
  print_endline " ]";

