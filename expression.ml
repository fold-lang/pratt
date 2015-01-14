
open Foundation

type t =
    | Application of (string list * t list)
    | Sequence of t list
    | Integer of int
    | Variable of string
    | Start of t
    | End


let integer a = Integer a
let variable a = Variable a
let start a = Start a
let fim = End

let rec show = function
    | Application (f, a) -> format "(%s %s)" (join ":" f)
                                             (join " " (List.map show a))
    | Sequence l -> "(seq)"
    | Integer             a -> format "%d" a
    | Variable            a -> a
    | Start               a -> show a
    | End                   -> "(end)"


