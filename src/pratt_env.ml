
open Elements

module Scope = Map.Make(String)

type 'a env = {
  data : 'a Scope.t;
  next : 'a env option;
}

module Env = struct
  type 'a t = 'a env

  let make next = { next;        data = Scope.empty }
  let empty     = { next = None; data = Scope.empty }

  let add name value env =
    { env with data = Scope.add name value env.data }

  let add_many env bindings =
    List.fold_left bindings ~init:env
      ~f:(fun env (n, v) -> add n v env)

  let rec find env name =
    if Scope.mem name env.data then
      Some env
    else begin
      match env.next with
      | Some next -> find next name
      | None -> None
    end

  let get name env =
    begin match find env name with
      | Some found_env -> Scope.find name found_env.data
      | None -> raise (Invalid_argument ("name " ^ name ^ " is not defined"))
    end

end

