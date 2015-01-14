
open Foundation

type t =
    | Nud of string list
    | Led of (string * t * t)

let empty = Nud []

let rec show = function
    | Nud xs -> format "(%s)" (join " " xs)
    | Led (x, l, r) -> format "(%s %s %s)" (show l) x (show r)


