
module Scope = Map.Make(String)

type 'a env = {
  next : 'a env option;
  data : 'a Scope.t;
}

module Env = struct
  type 'a t = 'a env

  let make next = { next;        data = Scope.empty }
  let empty     = { next = None; data = Scope.empty }

  let add name value env =
    { env with data = Scope.add name value env.data }

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

