
open Foundation

type t =
    | List of t list
    | Atom of string

let empty = List []

let append e1 e2 = (e1, e2) => function
    | List l1, List l2 -> List (l1 @ l2)
    | e, List l -> List (e::l)
    | List l, e -> List (l @ [e])
    | _, _ -> List [e1; e2]

let rec show = function
    | Atom x -> x
    | List l -> format "(%s)" (join " " (map show l))

