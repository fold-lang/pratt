
open Foundation

type t =
    | List of t list
    | Atom of string

let empty = List []

let append e1 e2 =
    match (e1, e2) with
    | List l1, List l2 -> List (l1 @ l2)
    | _, List l1 -> List (l1 @ [e1])
    | List l1, _ -> List (l1 @ [e2])
    | _, _ -> List [e1; e2]

let rec show = function
    | Atom x -> x
    | List l -> format "(%s)" (join " " (map show l))

