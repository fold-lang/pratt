

type t =
    | Symbol of string
    | Integer of int
    | Start
    | End


let show =
  function
  | Symbol x -> "(symbol " ^ x ^ ")"
  | Integer x -> "(int " ^ (string_of_int x) ^ ")"
  | Start -> "__start__"
  | End -> "__end__"


let is_end =
  function End -> true | _ -> false


let print t = print_endline (show t)

let print_list tl =
  print_string "[";
  List.iter (fun t -> print_string @@ " " ^ (show t)) tl;
  print_endline " ]";

