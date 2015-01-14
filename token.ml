
type t =
    | Letter of string
    | Symbol of string
    | End


let show = function
	| Letter x | Symbol x -> x
	| End -> "`end"
