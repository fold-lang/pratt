
open Foundation

module Scope = Map.Make(String)

type 'a t = ('a Scope.t list * 'a)

let init ~main ~default = ([main], default)

let show_scope s : string =
  "[%s]" % (join ", " (List.map fst (Scope.bindings s)))

let show g =
  join "::" (List.map show_scope (fst g))

let define (env, default) name handler =
  match env with
  | scope::env' -> ((Scope.add name handler scope)::env', default)
  | [] -> raise (Failure ("cannot define symbol %s: empty grammar" % name))

let rec lookup (env, default) name =
  match env with
  | scope::env' ->
      if Scope.mem name scope
        then Scope.find name scope
        else lookup (env', default) name
  | [] -> default

let new_scope (env, default) =
  (Scope.empty::env, default)

module Infix = struct
  let ( +> ) scope (name, handler) = Scope.add name handler scope
end

