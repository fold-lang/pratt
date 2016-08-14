
open Pure

let color_format color =
  "\027[%dm%s\027[0m" %
    match color with
    | `Black   -> 30
    | `Red     -> 31
    | `Green   -> 32
    | `Yellow  -> 33
    | `Blue    -> 34
    | `Magenta -> 35
    | `Cyan    -> 36
    | `White   -> 37

let blue               = color_format `Blue
let red                = color_format `Red
let yellow             = color_format `Yellow
let magenta            = color_format `Magenta
let cyan               = color_format `Cyan
let white              = color_format `White
let green              = color_format `Green
let bright_white x     = "\027[1;37m%s\027[0m" % x
let bright_blue x      = "\027[1;34m%s\027[0m" % x
let bright_magenta x   = "\027[1;35m%s\027[0m" % x
let violet x           = "\027[0;34m%s\027[0m" % x
let bright_red x       = "\027[1;31m%s\027[0m" % x
let bright_green x     = "\027[1;32m%s\027[0m" % x
let start_bright_white = "\027[1;37m"
let start_white        = "\027[37m"
let end_color          = "\027[0m"
let italic x           = "\027[3m" ^ x ^ "\027[0m"
let underline x        = "\027[4m" ^ x ^ "\027[0m"
let blink x            = "\027[5m" ^ x ^ "\027[0m"


let info    msg = print ~file:stderr (blue   "- " ^ msg)
let trace   msg = print ~file:stderr (cyan   "> " ^ msg)
let error   msg = print ~file:stderr (red    "* " ^ msg)
let warning msg = print ~file:stderr (yellow "! " ^ msg)


let compare' a b =
  match compare a b with
  | +1 -> `GT
  |  0 -> `EQ
  | -1 -> `LT
  | _  -> undefined ()

module Show = struct
  let int = string_of_int
  let float = string_of_float
  let list show_item l = "[%s]" % String.concat ", " (List.map show_item l)
  let assoc k v l = list (fun (x, y) -> "(%s, %s)" % (k x, v y)) l
  let option show_item opt =
    match opt with
    | Some item -> "Some %s" % show_item item
    | None -> "None"
end


module Assoc = struct
  type ('a, 'b) t = ('a * 'b) list
    [@@deriving show]

  let s = show
  let find t key =
    let rec loop = function
      | [] -> None
      | (k, v) :: l -> if k = key then Some v else loop l
    in
    loop t

  let mem t key = find t key <> None

  let remove t key =
    List.filter (fun (key', _) -> not (key = key')) t

  let add t key value =
    (key, value) :: remove t key

  let inverse t = List.map (fun (x, y) -> (y, x)) t

  let map f t = List.map (fun (key, value) -> (key, f value)) t
end

