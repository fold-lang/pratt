
open Foundation

type sym_desc = string

type t =
    | Sym of sym_desc
    | Nud of t list
    | Led of (t * t * t)

let empty = Nud []

let append e1 e2 =
    match (e1, e2) with
    | Nud a, Nud b -> Nud (a @ b)
    | _, Nud l -> Nud (l @ [e1])
    | Nud l, _ -> Nud (l @ [e2])
    | _, _ -> Nud [e1; e2]

let rec show = function
    | Sym x -> x
    | Nud xs -> format "[%s]" (join " " (map show xs))
    | Led (x, l, r) -> format "(%s %s %s)" (show l) (show x) (show r)

